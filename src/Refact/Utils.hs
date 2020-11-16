{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE RankNTypes  #-}

module Refact.Utils ( -- * Synonyms
                      Module
                    , Stmt
                    , Expr
                    , Decl
                    , Name
                    , Pat
                    , Type
                    , Import
                    , FunBind
                    -- * Monad
                    , M
                    -- * Utility

                    , mergeAnns
                    , modifyAnnKey
                    , replaceAnnKey
                    , toGhcSrcSpan
                    , setSrcSpanFile
                    , findParent

                    ) where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types

import Data.Data

import FastString (FastString)
import SrcLoc
import qualified SrcLoc as GHC
import qualified RdrName as GHC
import qualified ApiAnnotation as GHC
import qualified FastString    as GHC
import qualified GHC hiding (parseModule)

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Expr as GHC hiding (Stmt)
import GHC.Hs.ImpExp
#else
import HsExpr as GHC hiding (Stmt)
import HsImpExp
#endif

import Control.Monad.Trans.State

import qualified Data.Map as Map
import Data.Maybe


import qualified Refact.Types as R

import Data.Generics.Schemes
import Unsafe.Coerce


-- Types
--
type M a = StateT Anns IO a

type Module = (GHC.Located (GHC.HsModule GHC.GhcPs))

type Expr = GHC.Located (GHC.HsExpr GHC.GhcPs)

type Type = GHC.Located (GHC.HsType GHC.GhcPs)

type Decl = GHC.Located (GHC.HsDecl GHC.GhcPs)

type Pat =  GHC.Located (GHC.Pat GHC.GhcPs)

type Name = GHC.Located GHC.RdrName

type Stmt = ExprLStmt GHC.GhcPs

type Import = LImportDecl GHC.GhcPs

type FunBind = HsMatchContext GHC.RdrName

-- | Replaces an old expression with a new expression
--
-- Note that usually, new, inp and parent are all the same.
replace :: AnnKey  -- The thing we are replacing
        -> AnnKey  -- The thing which has the annotations we need for the new thing
        -> AnnKey  -- The thing which is going to be inserted
        -> AnnKey  -- The "parent", the largest thing which has he same SrcSpan
                   -- Usually the same as inp and new
        -> Anns -> Maybe Anns
replace old new inp parent anns = do
  oldan <- Map.lookup old anns
  newan <- Map.lookup new anns
  oldDelta <- annEntryDelta  <$> Map.lookup parent anns
  return $ Map.insert inp (combine oldDelta new oldan newan) anns

combine :: DeltaPos -> AnnKey -> Annotation -> Annotation -> Annotation
combine oldDelta newkey oldann newann =
  Ann { annEntryDelta = newEntryDelta
      , annPriorComments = annPriorComments oldann ++ annPriorComments newann
      , annFollowingComments = annFollowingComments oldann ++ annFollowingComments newann
      , annsDP = removeComma (annsDP newann) ++ extraComma (annsDP oldann)
      , annSortKey = annSortKey newann
      , annCapturedSpan = annCapturedSpan newann}
  where
    -- Get rid of structural information when replacing, we assume that the
    -- structural information is already there in the new expression.
    removeComma = filter (\(kw, _) -> case kw of
                                         G GHC.AnnComma
                                           | AnnKey _ (CN "ArithSeq") <- newkey -> True
                                           | otherwise -> False
                                         AnnSemiSep -> False
                                         _ -> True)

    -- Make sure to keep structural information in the template.
    extraComma [] = []
    extraComma (last -> x) = case fst x of
                              G GHC.AnnComma -> [x]
                              AnnSemiSep -> [x]
                              G GHC.AnnSemi -> [x]
                              _ -> []

    -- Keep the same delta if moving onto a new row
    newEntryDelta | deltaRow oldDelta > 0 = oldDelta
                  | otherwise = annEntryDelta oldann


-- | A parent in this case is an element which has the same SrcSpan
findParent :: Data a => GHC.SrcSpan -> Anns -> a -> Maybe AnnKey
findParent ss as = something (findParentWorker ss as)

-- Note that a parent must also have an annotation.
findParentWorker :: forall a . (Data a)
           => GHC.SrcSpan -> Anns -> a -> Maybe AnnKey
findParentWorker oldSS as a
  | con == typeRepTyCon (typeRep (Proxy :: Proxy (GHC.Located GHC.RdrName))) && x == typeRep (Proxy :: Proxy GHC.SrcSpan)
      = if ss == oldSS
            && isJust (Map.lookup (AnnKey ss cn) as)
          then Just $ AnnKey ss cn
          else Nothing
  | otherwise = Nothing
  where
    (con, ~[x, _]) = splitTyConApp (typeOf a)
    ss :: GHC.SrcSpan
    ss = gmapQi 0 unsafeCoerce a
    cn = gmapQi 1 (CN . show . toConstr) a


-- | Perform the necessary adjustments to annotations when replacing
-- one Located thing with another Located thing.
--
-- For example, this function will ensure the correct relative position and
-- make sure that any trailing semi colons or commas are transferred.
modifyAnnKey
  :: (Data old, Data new, Data mod)
  => mod -> Located old -> Located new -> M (Located new)
modifyAnnKey m e1 e2 = do
  as <- get
  let parentKey = fromMaybe (mkAnnKey e2) (findParent (getLoc e2) as m)
  e2 <$ modify ( recoverBackquotes e1 e2
               . replaceAnnKey (mkAnnKey e1) (mkAnnKey e2) (mkAnnKey e2) parentKey
               )

-- | When the template contains a backquoted substitution variable, but the substitute
-- is not backquoted, we must add the corresponding 'GHC.AnnBackQuote's.
--
-- See tests/examples/Backquotes.hs for an example.
recoverBackquotes :: Located old -> Located new -> Anns -> Anns
recoverBackquotes (L old _) (L new _) anns
  | Just annOld <- Map.lookup (AnnKey old (CN "Unqual")) anns
  , ( (G GHC.AnnBackquote, DP (i, j))
    : rest@( (G GHC.AnnVal, _)
           : (G GHC.AnnBackquote, _)
           : _)
    ) <- annsDP annOld
  = let f annNew = case annsDP annNew of
          [(G GHC.AnnVal, DP (i', j'))] ->
            annNew {annsDP = (G GHC.AnnBackquote, DP (i + i', j + j')) : rest}
          _ -> annNew
     in Map.adjust f (AnnKey new (CN "Unqual")) anns
  | otherwise = anns

-- | Lower level version of @modifyAnnKey@
replaceAnnKey :: AnnKey -> AnnKey -> AnnKey -> AnnKey -> Anns -> Anns
replaceAnnKey old new inp deltainfo a =
  fromMaybe a (replace old new inp deltainfo a)


-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan :: FilePath -> R.SrcSpan -> SrcSpan
toGhcSrcSpan file R.SrcSpan{..} = mkSrcSpan (f startLine startCol) (f endLine endCol)
  where
    f = mkSrcLoc (GHC.mkFastString file)

setSrcSpanFile :: FastString -> SrcSpan -> SrcSpan
setSrcSpanFile file s
  | RealSrcLoc start <- srcSpanStart s
  , RealSrcLoc end <- srcSpanEnd s
  = let start' = mkSrcLoc file (srcLocLine start) (srcLocCol start)
        end' = mkSrcLoc file (srcLocLine end) (srcLocCol end)
     in mkSrcSpan start' end'
setSrcSpanFile _ s = s
