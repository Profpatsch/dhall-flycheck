{-# language NamedFieldPuns, OverloadedStrings #-}
module Dhall.Flycheck where

import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Data.Text.IO
import qualified System.Environment
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Error
import qualified Text.Megaparsec.Pos
import qualified Text.Megaparsec.Stream
import Data.Text (Text)
import qualified Data.Text
import Data.Foldable
import Data.Bifunctor (first, bimap)
import Control.Monad.Except (ExceptT(..), runExceptT)
import qualified Control.Exception
import qualified Data.Sequence
import Data.Sequence (Seq)
import Data.Void
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString.Lazy as LBS

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

main :: IO ()
main = do
  args <- System.Environment.getArgs
  let filename = case args of
        [] -> error "no args"
        [fn] -> fn
        _ -> error "only one arg!"

  contents <- Data.Text.IO.readFile filename

  errsE <- editorErrors filename contents
  case errsE of
    (Right _) -> return ()
    (Left errs) ->
      LBS.putStr
        $ editorErrorsToJson $ toList errs
  return ()

  where
    editorErrors :: FilePath -> Text -> IO (Either (Seq EditorError) ())
    editorErrors sourceFileName sourceContent = runExceptT $ do
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
          EditorError
            -- TODO: actually find out correct positions of failing
            -- imports by stepping through the AST and trying to load
            -- each import in order.
            -- see https://github.com/dhall-lang/dhall-haskell/issues/561
            { editorPos = Text.Megaparsec.Pos.initialPos filename
            , editorMessage =
                Data.Foldable.foldMap
                  (\e ->
                     "\n"
                     <> removeEscapes (Data.Text.pack (show e))
                     <> "\n")
                  es
            }

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
