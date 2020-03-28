{-# language NamedFieldPuns, OverloadedStrings, LambdaCase, BangPatterns #-}
module Dhall.Flycheck where

import qualified Dhall.Core
import Dhall.Core
  ( Expr(Note)
  , Import(Import), ImportType(Missing, Env, Local, Remote)
  , ImportHashed(ImportHashed), importHashed, importType
  )
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.Map
import Dhall.Parser (Src)
import qualified Dhall.TypeCheck
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding
import qualified System.Environment
import qualified System.IO as SIO
import qualified System.Exit
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Error
import qualified Text.Megaparsec.Pos
import qualified Text.Megaparsec.Stream
import Data.Text (Text)
import qualified Data.Text
import Data.Foldable
import Data.Bifunctor (first, bimap)
import Control.Monad.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Exception
import Control.Monad.Trans.Class (lift)
import qualified Data.Sequence
import Data.Sequence (Seq)
import Data.Void
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString as BS
import qualified Lens.Family as Lens

data ParseError = ParseError
  { parseErrorPos :: Text.Megaparsec.Pos.SourcePos
    -- ^ line and column where the parse error begins
  , parseBadToken :: Text
    -- ^ highlighted token that starts at sourcePos
  , parseMessage :: Text
    -- ^ formatted error message
  }

data EditorError = EditorError
  { editorPos :: Text.Megaparsec.Pos.SourcePos
  , editorMessage :: Text
  }

editorErrorNoPos :: FilePath -> Text -> EditorError
editorErrorNoPos filename editorMessage = EditorError
  { editorPos = Text.Megaparsec.Pos.initialPos filename
  , editorMessage }


parseErrorToEditorError :: ParseError -> EditorError
parseErrorToEditorError pe = EditorError
  { editorPos = parseErrorPos pe
  , editorMessage = parseMessage pe }

editorErrorsToJson :: [EditorError] -> LBS.ByteString
editorErrorsToJson ees =
  let
      one ee = Json.pairs
        ( "filename" Json..= Text.Megaparsec.Pos.sourceName pos
        <> "line" Json..= Text.Megaparsec.Pos.unPos
                            (Text.Megaparsec.Pos.sourceLine pos)
        <> "column" Json..= Text.Megaparsec.Pos.unPos
                              (Text.Megaparsec.Pos.sourceColumn pos)
        <> "message" Json..= editorMessage ee )
        where pos = editorPos ee
  in Json.encodingToLazyByteString $ Json.list one ees

usage :: [Char]
usage = mconcat
  [ "Usage:\n"
  , "  dhall-flycheck <filename>\n"
  , "  dhall-flycheck --forever\n"
  , "\n"
  , "Checks dhall code in three consecutive steps, first"
  , " syntax, then imports, then types.\n"
  , "If any step fails, it returns a JSON that can be used by an"
  , " in-editor checker.\n"
  , "\n"
  , "In the first form, it accepts a filename and does one single check\n"
  , "In the second form, it accepts a filename per line on stdin"
  , " and outputs one JSON per line on stdout (line-buffered).\n"
  , "It the second form, it caches imports to speed up checks.\n"
  ]

type Cache = Dhall.Map.Map Dhall.Import.Chained Dhall.Import.ImportSemantics

emptyCache :: Cache
emptyCache = mempty

main :: IO ()
main = do
  args <- System.Environment.getArgs
  let usg = System.Exit.die usage
  case args of
        [] -> usg
        ["--help"] -> usg
        ["--forever"] -> absurd <$> checkForever
        [fn] -> checkOne fn
        _ -> usg

  where
    -- | check file, print errors, return
    checkOne :: FilePath -> IO ()
    checkOne filename = do
      _ <- printCheckResult (pure ()) emptyCache filename
      pure ()

    -- | Dhall.Map doesn’t have this atm, so workaround
    mapMaybeWithKey :: Ord k => (k -> v -> Maybe v) -> Dhall.Map.Map k v -> Dhall.Map.Map k v
    mapMaybeWithKey f m =
       Dhall.Map.toList m
       & mapMaybe (\(k, v) -> (,) <$> pure k <*> f k v)
       & Dhall.Map.fromList

    -- | loop forever, get filenames from stdin,
    -- print one error list per input to stdout
    checkForever :: IO Void
    checkForever = do
      -- set up stdin and stdout
      SIO.hSetBuffering SIO.stdout SIO.LineBuffering
      SIO.hSetBuffering SIO.stdin SIO.LineBuffering
      -- we read filenames, which are Bytestrings
      SIO.hSetBinaryMode SIO.stdin True
      -- the json is also output as Bytestring
      SIO.hSetBinaryMode SIO.stdout True

      -- endless loop IO action, feeding previous result
      let foreverFeed feed act = do
            !newFeed <- act feed
            foreverFeed newFeed act

      foreverFeed emptyCache $ \cache -> do
        newCache <- SIO.hGetLine SIO.stdin
          >>= printCheckResult (LBSC.putStrLn "") cache
        -- we don’t want to keep all imports in the memory cache,
        -- only a certain (expensive) subset
        pure $ mapMaybeWithKey (\imp val -> if cacheImportPred (Dhall.Import.chainedImport imp) then Just val else Nothing) newCache

    -- | Predicate that returns whether an import should be
    -- | cached in-memory.
    cacheImportPred :: Import -> Bool
    cacheImportPred
      (Import { importHashed = ImportHashed { importType } })
      = case importType of
          -- we always cache remote urls, no matter whether they
          -- are fixed or not. If the user wants to re-download
          -- them, they should restart the dhall-flycheck process.
          Remote _ -> True
          Missing -> False
          Env _ -> False
          -- TODO: investigate whether hashing the file contents
          -- of local files speeds up things
          Local _ _ -> False

    -- | check for filename and print json to stdout
    printCheckResult
      :: IO ()
      -- ^ what to do when there was no error
      -> Cache
      -- ^ in-memory cache of imports
      -> FilePath
      -> IO Cache
      -- ^ the new import cache
    printCheckResult noError cache filename = do
      State.runStateT (checkFile filename) cache
        >>= \(res, newCache) -> do
          case res of
            Nothing -> noError
            Just errs -> LBSC.putStrLn $ editorErrorsToJson $ toList errs
          pure newCache

    -- | Reads a file, can throw a decoding error first
    checkFile :: FilePath
              -> State.StateT Cache IO (Maybe (Seq EditorError))
    checkFile filename = do
      lift (Data.Text.Encoding.decodeUtf8' <$> BS.readFile filename) >>= \case
          Left _ -> pure $ Just $ Data.Sequence.singleton
            $ editorErrorNoPos filename
              $  "Cannot decode "
              <> (Data.Text.pack filename)
              <> ", it is not valid UTF-8"
          Right contents -> do
            oldCache <- State.get
            State.mapStateT
              -- this is a bit confused, rebasing ExceptT to Maybe,
              -- and both inside of StateT …
              (\ext -> runExceptT ext >>= \case
                  Left errs -> pure (Just errs, oldCache)
                  Right ((), cache) -> pure (Nothing, cache))
              (editorErrors filename contents)

    -- | Do syntax check, imports and type check and collect
    -- all errors and a cache of imported files
    editorErrors :: FilePath
                 -- ^ name of source file
                 -> Text
                 -- ^ contents of source file
                 -> State.StateT Cache (ExceptT (Seq EditorError) IO) ()
                 -- ^ Possible errors and updated import cache
    editorErrors sourceFileName sourceContent = do
      let wrap = lift . ExceptT . pure
      parsed <- wrap $ doParse sourceFileName sourceContent
      imports <- State.StateT
        (\oldCache -> ExceptT $ doImports sourceFileName oldCache parsed)
      wrap $ doTypeCheck imports

    -- | parse the file
    doParse :: FilePath -> Text
            -> Either (Seq EditorError)
                 (Expr Src Import)
    doParse filename contents =
      first (fmap parseErrorToEditorError . convertMegaParseErrors)
        $ Dhall.Parser.exprFromText filename contents

    -- | Like Dhall.Import.load, but initialized with a cache
    -- of all imported files we’ve loaded (from disk or network)
    -- in previous runs of the checker.
    -- This considerably speeds up the checking process by about
    -- one order of magnitude for locally cached files and two
    -- orders of magnitude for network files.
    loadWithPreviousCache
      :: Cache -> Expr Src Import -> IO (Expr Src Void, Cache)
    loadWithPreviousCache previousCache expression = do
      (newExpression, newCache) <-
        State.runStateT
          (Dhall.Import.loadWith expression)
          -- TODO: Is "." correct?
          (Dhall.Import.emptyStatus "."
            Lens.& Dhall.Import.cache Lens..~ previousCache)
      pure (newExpression, newCache Lens.^. Dhall.Import.cache)

    -- | Collect import errors and use the in-memory cache.
    doImports :: FilePath
              -> Cache
              -> (Expr Dhall.Parser.Src Import)
              -> IO (Either (Seq EditorError)
                   (Expr Dhall.Parser.Src Void, Cache))
    doImports filename oldCache parsed = do
      Control.Exception.try $ do
        loadWithPreviousCache oldCache parsed
      >>= return . first (Data.Sequence.singleton . fromMissingImports)
      where
        fromMissingImports (Dhall.Import.MissingImports es) =
          -- TODO: actually find out correct positions of failing
          -- imports by stepping through the AST and trying to load
          -- each import in order.
          -- see https://github.com/dhall-lang/dhall-haskell/issues/561
          editorErrorNoPos filename
            $ Data.Foldable.foldMap
                (\e ->
                    "\n"
                    <> removeEscapes (Data.Text.pack (show e))
                    <> "\n")
                es

    -- | Collect type check errors.
    doTypeCheck :: (Expr Dhall.Parser.Src Void)
                -> Either (Seq EditorError) ()
    doTypeCheck dhallExpr =
      bimap
        (Data.Sequence.singleton . typeErrorToEditorError)
        (const ())
        $ Dhall.TypeCheck.typeOf dhallExpr

-- bad hack to work around the error message
-- terminal escape sequences
removeEscapes :: Text -> Text
removeEscapes =
    Data.Text.replace "\ESC[0m" ""
  . Data.Text.replace "\ESC[1;31m" ""

-- TODO: long messages?
-- | Convert type error messages to something we can use.
typeErrorToEditorError
  :: Dhall.TypeCheck.TypeError Dhall.Parser.Src Void
  -> EditorError
typeErrorToEditorError (Dhall.TypeCheck.TypeError context expr typeMessage) =
  EditorError { editorPos, editorMessage }
  where
    (editorPos, unNotedExpr) = case expr of
      (Note
        -- TODO: use the whole span instead of just the
        -- initial SrcPos.
        (Dhall.Parser.Src from _to _text) unNoted) -> (from, unNoted)
      other ->
        error $ Data.Text.unpack
          $ "unexpected expr, should be Note:\n"
          <> Dhall.Core.pretty other

    editorMessage =
      -- We abuse the Pretty instance of typeError,
      -- basing it on the fact that the source position
      -- is not printed when we remove the Note from expr
      removeEscapes
        $ Dhall.Core.pretty
          $ Dhall.TypeCheck.TypeError
              { Dhall.TypeCheck.context
              , Dhall.TypeCheck.typeMessage
              , Dhall.TypeCheck.current = unNotedExpr }

-- | Convert parser errors to something we can use.
convertMegaParseErrors
  :: Dhall.Parser.ParseError
  -> Seq ParseError
convertMegaParseErrors dhallParseError =
  let megaErrorBundle = Dhall.Parser.unwrap dhallParseError

      megaErrors = Text.Megaparsec.Error.bundleErrors megaErrorBundle
      megaPosState = Text.Megaparsec.Error.bundlePosState megaErrorBundle

      forPosState :: (Seq ParseError, Text.Megaparsec.PosState Text)
                  -> Text.Megaparsec.Error.ParseError Text Void
                  -> (Seq ParseError, Text.Megaparsec.PosState Text)
      forPosState (errs, posState) megaError =
        let (pos, badToken, posState') =
              Text.Megaparsec.Stream.reachOffset
                (Text.Megaparsec.Error.errorOffset megaError)
                posState
            err = ParseError
              { parseErrorPos = pos
              , parseBadToken = Data.Text.pack badToken
              , parseMessage =
                  Data.Text.pack
                    $ Text.Megaparsec.Error.parseErrorTextPretty
                        megaError
              }
            errs' = errs Data.Sequence.:|> err
        in (errs', posState')

      (parseErrors, _) =
        foldl'
          forPosState
          (Data.Sequence.empty, megaPosState)
          megaErrors

  in parseErrors
