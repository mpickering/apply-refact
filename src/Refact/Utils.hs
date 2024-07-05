{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Refact.Utils
  ( -- * Synonyms
    Module,
    CompleteModule (..),
    Stmt,
    Expr,
    Decl,
    Name,
    Pat,
    Type,
    Import,
    FunBind,
    pattern RealSrcLoc',
    pattern RealSrcSpan',
    DoGenReplacement,
    ReplaceWorker,

    -- * Monad
    M,

    -- * Utility
    modifyAnnKey,
    getAnnSpan,
    getAnnSpanA,
    toGhcSrcSpan,
    toGhcSrcSpan',
    annSpanToSrcSpan,
    srcSpanToAnnSpan,
    setAnnSpanFile,
    setSrcSpanFile,
    setRealSrcSpanFile,
  )
where

import Control.Monad.Trans.State.Strict (StateT)
import Data.Data
  ( Data (),
  )
import Data.Generics (everywhere, mkT)
import Data.Typeable
import qualified GHC
import Language.Haskell.GHC.ExactPrint.Parsers (Parser)
import Refact.Compat
  ( AnnSpan,
    FastString,
    FunBind,
    Module,
    annSpanToSrcSpan,
    fromSrcSpanAnn,
    toSrcSpanAnn,
    mkFastString,
    setAnnSpanFile,
    setRealSrcSpanFile,
    setSrcSpanFile,
    srcSpanToAnnSpan,
    pattern RealSrcLoc',
    pattern RealSrcSpan',
    transferEntryDP,
  )
import qualified Refact.Types as R

-- Types
-- type M a = StateT (Anns, AnnKeyMap) IO a
type M a = StateT () IO a

type Expr = GHC.LHsExpr GHC.GhcPs

type Type = GHC.LHsType GHC.GhcPs

type Decl = GHC.LHsDecl GHC.GhcPs

type Pat = GHC.LPat GHC.GhcPs

type Name = GHC.LocatedN GHC.RdrName

type Stmt = GHC.ExprLStmt GHC.GhcPs

type Import = GHC.LImportDecl GHC.GhcPs

data CompleteModule = CompleteModule
  { cmSpan :: Module
  , cmDelta :: Module
  }

type DoGenReplacement an ast =
  (Data ast) =>
  Int ->
  GHC.LocatedAn an ast ->
  GHC.LocatedAn an ast ->
  StateT (Bool, Int) IO (GHC.LocatedAn an ast)

type ReplaceWorker a =
  (Data a) =>
  CompleteModule ->
  Parser (GHC.LocatedA a) ->
  Int ->
  R.Refactoring GHC.SrcSpan ->
  IO CompleteModule

getAnnSpanA :: forall an a. GHC.LocatedAn an a -> AnnSpan
getAnnSpanA = srcSpanToAnnSpan . GHC.getLocA

getAnnSpan :: forall a. GHC.Located a -> AnnSpan
getAnnSpan = srcSpanToAnnSpan . GHC.getLoc

-- | Perform the necessary adjustments to annotations when replacing
-- one Located thing with another Located thing.
--
-- For example, this function will ensure the correct relative position and
-- make sure that any trailing semi colons or commas are transferred.
-- modifyAnnKey ::
--   (Data old, Data new, Data mod) =>
--   mod ->
--   GHC.Located old ->
--   GHC.Located new ->
--   M (GHC.Located new)
modifyAnnKey ::
  (Data mod, Data t, Data old, Data new, Monoid t, Typeable t) =>
  mod ->
  GHC.LocatedAn t old ->
  GHC.LocatedAn t new ->
  M (GHC.LocatedAn t new)
modifyAnnKey _m e1 e2 = do
  -- liftIO $ putStrLn $ "modifyAnnKey:e1" ++ showAst e1
  -- liftIO $ putStrLn $ "modifyAnnKey:e2" ++ showAst e2
  let e2_0 = handleBackquotes e1 e2
  -- liftIO $ putStrLn $ "modifyAnnKey:e2_0" ++ showAst e2_0
  let e2' = transferEntryDP e1 e2_0
  -- liftIO $ putStrLn $ "modifyAnnKey:e2'" ++ showAst e2'
  pure e2'

-- | This function handles backquotes in two scenarios:
--
--     1. When the template contains a backquoted substitution variable, but the substitute
--        is not backquoted, we must add the corresponding 'GHC.NameBackquotes'. See
--        tests/examples/Backquotes.hs for an example.
--     2. When the template contains a substitution variable without backquote, and the
--        substitute is backquoted, we remove the 'GHC.NameBackquotes' annotation. See
--        tests/examples/Uncurry.hs for an example.
--        N.B.: this is not always correct, since it is possible that the refactoring output
--        should keep the backquotes, but currently no test case fails because of it.
handleBackquotes ::
  forall t old new.
  (Data t, Data old, Data new, Typeable t) =>
  GHC.LocatedAn t old ->
  GHC.LocatedAn t new ->
  GHC.LocatedAn t new
handleBackquotes old new@(GHC.L loc _) =
  everywhere (mkT update) new
  where
    update :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
    update (GHC.L l (GHC.HsVar x (GHC.L ln n))) = GHC.L l (GHC.HsVar x (GHC.L ln' n))
      where
        ln' =
          if GHC.locA l == GHC.locA loc
            then case cast old :: Maybe (GHC.LHsExpr GHC.GhcPs) of
              Just (GHC.L _ (GHC.HsVar _ (GHC.L (fromSrcSpanAnn -> (GHC.EpAnn _ ann _, _)) _)))
                -- scenario 1
                | GHC.NameAnn GHC.NameBackquotes _ _ _ _ <- ann ->
                  case fromSrcSpanAnn ln of
                    (GHC.EpAnn a _ cs, ll) -> toSrcSpanAnn (GHC.EpAnn a ann cs) ll
#if MIN_VERSION_ghc(9,10,0)
#else
                    (GHC.EpAnnNotUsed, ll) ->
                      toSrcSpanAnn (GHC.EpAnn (GHC.spanAsAnchor ll) ann GHC.emptyComments) ll
#endif
                -- scenario 2
                | (GHC.EpAnn a ann' cs, ll) <- fromSrcSpanAnn ln,
                  GHC.NameAnn GHC.NameBackquotes _ _ _ _ <- ann' ->
                  toSrcSpanAnn (GHC.EpAnn a ann cs) ll
              Just _ -> ln
              Nothing -> ln
            else ln
    update x = x

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan :: FilePath -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan = toGhcSrcSpan' . mkFastString

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan' :: FastString -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan' file R.SrcSpan {..} = GHC.mkSrcSpan (f startLine startCol) (f endLine endCol)
  where
    f = GHC.mkSrcLoc file
