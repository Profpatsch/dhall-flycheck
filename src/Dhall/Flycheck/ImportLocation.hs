module Dhall.Flycheck.ImportLocation where

import Dhall.Core
import Dhall.Parser
import Dhall.Import
import Data.Sequence (Seq)
import qualified Data.Sequence
import Control.Exception (catch)

-- badImports :: Expr Src Import -> IO (Seq Dhall.Parser.Src)
-- badImports expr = case expr of
--   Embed import_ -> undefined
--   ImportAlt a b -> loadWith a `catch` handler₀
--     where
--       handler₀ (MissingImports es₀) =
--         loadWith b `catch` handler₁
--         where
--           handler₁ (MissingImports es₁) =
--             throwM (MissingImports (es₀ ++ es₁))
--   Const a              -> pure mempty
--   Var a                -> pure mempty
--   Lam a b c            -> Lam <$> pure a <*> loadWith b <*> loadWith c
--   Pi a b c             -> Pi <$> pure a <*> loadWith b <*> loadWith c
--   App a b              -> App <$> loadWith a <*> loadWith b
--   Let as b             -> Let <$> traverse f as <*> loadWith b
--     where
--       f (Binding c d e) = Binding c <$> traverse loadWith d <*> loadWith e
--   Annot a b            -> Annot <$> loadWith a <*> loadWith b
--   Bool                 -> pure mempty
--   BoolLit a            -> pure mempty
--   BoolAnd a b          -> BoolAnd <$> loadWith a <*> loadWith b
--   BoolOr a b           -> BoolOr <$> loadWith a <*> loadWith b
--   BoolEQ a b           -> BoolEQ <$> loadWith a <*> loadWith b
--   BoolNE a b           -> BoolNE <$> loadWith a <*> loadWith b
--   BoolIf a b c         -> BoolIf <$> loadWith a <*> loadWith b <*> loadWith c
--   Natural              -> pure mempty
--   NaturalLit a         -> pure mempty
--   NaturalFold          -> pure mempty
--   NaturalBuild         -> pure mempty
--   NaturalIsZero        -> pure mempty
--   NaturalEven          -> pure mempty
--   NaturalOdd           -> pure mempty
--   NaturalToInteger     -> pure mempty
--   NaturalShow          -> pure mempty
--   NaturalPlus a b      -> NaturalPlus <$> loadWith a <*> loadWith b
--   NaturalTimes a b     -> NaturalTimes <$> loadWith a <*> loadWith b
--   Integer              -> pure Integer
--   IntegerLit a         -> pure (IntegerLit a)
--   IntegerShow          -> pure IntegerShow
--   IntegerToDouble      -> pure IntegerToDouble
--   Double               -> pure Double
--   DoubleLit a          -> pure (DoubleLit a)
--   DoubleShow           -> pure DoubleShow
--   Text                 -> pure Text
--   TextLit (Chunks a b) -> fmap TextLit (Chunks <$> mapM (mapM loadWith) a <*> pure b)
--   TextAppend a b       -> TextAppend <$> loadWith a <*> loadWith b
--   List                 -> pure List
--   ListLit a b          -> ListLit <$> mapM loadWith a <*> mapM loadWith b
--   ListAppend a b       -> ListAppend <$> loadWith a <*> loadWith b
--   ListBuild            -> pure ListBuild
--   ListFold             -> pure ListFold
--   ListLength           -> pure ListLength
--   ListHead             -> pure ListHead
--   ListLast             -> pure ListLast
--   ListIndexed          -> pure ListIndexed
--   ListReverse          -> pure ListReverse
--   Optional             -> pure Optional
--   None                 -> pure None
--   Some a               -> Some <$> loadWith a
--   OptionalLit a b      -> OptionalLit <$> loadWith a <*> mapM loadWith b
--   OptionalFold         -> pure OptionalFold
--   OptionalBuild        -> pure OptionalBuild
--   Record a             -> Record <$> mapM loadWith a
--   RecordLit a          -> RecordLit <$> mapM loadWith a
--   Union a              -> Union <$> mapM loadWith a
--   UnionLit a b c       -> UnionLit <$> pure a <*> loadWith b <*> mapM loadWith c
--   Combine a b          -> Combine <$> loadWith a <*> loadWith b
--   CombineTypes a b     -> CombineTypes <$> loadWith a <*> loadWith b
--   Prefer a b           -> Prefer <$> loadWith a <*> loadWith b
--   Merge a b c          -> Merge <$> loadWith a <*> loadWith b <*> mapM loadWith c
--   Constructors a       -> Constructors <$> loadWith a
--   Field a b            -> Field <$> loadWith a <*> pure b
--   Project a b          -> Project <$> loadWith a <*> pure b
--   Note a b             -> Note <$> pure a <*> loadWith b
