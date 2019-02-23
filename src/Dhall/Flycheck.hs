{-# language NamedFieldPuns, OverloadedStrings, LambdaCase #-}
module Dhall.Flycheck where

import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Data.Text.IO
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
import Control.Monad.Except (ExceptT(..), runExceptT, forever)
import qualified Control.Exception
import qualified Data.Sequence
import Data.Sequence (Seq)
import Data.Void
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString as BS

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
      checkFile filename >>= \case
        Nothing -> pure ()
        Just errs -> LBSC.putStrLn $ editorErrorsToJson $ toList errs

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

      forever $ do
        filename <- SIO.hGetLine SIO.stdin
        checkFile filename >>= \case
          Nothing -> LBSC.putStrLn ""
          Just errs -> LBSC.putStrLn $ editorErrorsToJson $ toList errs

    -- | Reads a file, can throw a decoding error first
    checkFile :: FilePath -> IO (Maybe (Seq EditorError))
    checkFile filename = do
      Data.Text.Encoding.decodeUtf8' <$> BS.readFile filename >>= \case
          Left _ -> pure $ Just $ Data.Sequence.singleton
            $ editorErrorNoPos filename
              $  "Cannot decode "
              <> (Data.Text.pack filename)
              <> ", it is not valid UTF-8"
          Right c -> do
            editorErrors filename c

    may :: Either e () -> Maybe e
    may (Left e) = Just e
    may (Right ()) = Nothing

    editorErrors :: FilePath -> Text -> IO (Maybe (Seq EditorError))
    editorErrors sourceFileName sourceContent = fmap may $ runExceptT $ do
      let wrap = ExceptT . pure
      parsed <- wrap $ doParse sourceFileName sourceContent
      imports <- ExceptT $ doImports sourceFileName parsed
      wrap $ doTypeCheck imports

    doParse :: FilePath -> Text
            -> Either (Seq EditorError)
                 (Dhall.Core.Expr Dhall.Parser.Src Dhall.Core.Import)
    doParse filename contents =
      first (fmap parseErrorToEditorError . convertMegaParseErrors)
        $ Dhall.Parser.exprFromText filename contents

    doImports :: FilePath
              -> (Dhall.Core.Expr Dhall.Parser.Src Dhall.Core.Import)
              -> IO (Either (Seq EditorError)
                   (Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X))
    doImports filename parsed = do
      Control.Exception.try $ Dhall.Import.load parsed
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

    doTypeCheck :: (Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X)
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
typeErrorToEditorError
  :: Dhall.TypeCheck.TypeError Dhall.Parser.Src Dhall.TypeCheck.X
  -> EditorError
typeErrorToEditorError (Dhall.TypeCheck.TypeError context expr typeMessage) =
  EditorError { editorPos, editorMessage }
  where
    (editorPos, unNotedExpr) = case expr of
      (Dhall.Core.Note
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
