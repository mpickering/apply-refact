{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
                    , AnnKeyMap
                    , pattern RealSrcLoc'
                    , pattern RealSrcSpan'
                    -- * Monad
                    , M
                    -- * Utility

                    , mergeAnns
                    , modifyAnnKey
                    , replaceAnnKey
                    , getAnnSpan
                    , toGhcSrcSpan
                    , toGhcSrcSpan'
                    , annSpanToSrcSpan
                    , srcSpanToAnnSpan
                    , setAnnSpanFile
                    , setSrcSpanFile
                    , setRealSrcSpanFile
                    , findParent
                    ) where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types

import Data.Bifunctor (bimap)
import Data.Data
import Data.Map.Strict (Map)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Data.FastString (FastString)
import qualified GHC.Data.FastString as GHC
import qualified GHC.Parser.Annotation as GHC
import qualified GHC.Types.Name.Reader as GHC
import GHC.Types.SrcLoc
import qualified GHC.Types.SrcLoc as GHC
#else
import FastString (FastString)
import qualified FastString as GHC
import SrcLoc
import qualified SrcLoc as GHC
import qualified RdrName as GHC
import qualified ApiAnnotation as GHC
#endif

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
type M a = StateT (Anns, AnnKeyMap) IO a

type AnnKeyMap = Map AnnKey [AnnKey]

#if __GLASGOW_HASKELL__ >= 900
type Module = (GHC.Located GHC.HsModule)
#else
type Module = (GHC.Located (GHC.HsModule GHC.GhcPs))
#endif

type Expr = GHC.Located (GHC.HsExpr GHC.GhcPs)

type Type = GHC.Located (GHC.HsType GHC.GhcPs)

type Decl = GHC.Located (GHC.HsDecl GHC.GhcPs)

type Pat =  GHC.Located (GHC.Pat GHC.GhcPs)

type Name = GHC.Located GHC.RdrName

type Stmt = ExprLStmt GHC.GhcPs

type Import = LImportDecl GHC.GhcPs

#if __GLASGOW_HASKELL__ >= 900
type FunBind = HsMatchContext GHC.GhcPs
#else
type FunBind = HsMatchContext GHC.RdrName
#endif

pattern RealSrcLoc' :: RealSrcLoc -> SrcLoc
#if __GLASGOW_HASKELL__ >= 900
pattern RealSrcLoc' r <- RealSrcLoc r _ where
  RealSrcLoc' r = RealSrcLoc r Nothing
#else
pattern RealSrcLoc' r <- RealSrcLoc r where
  RealSrcLoc' r = RealSrcLoc r
#endif
{-# COMPLETE RealSrcLoc', UnhelpfulLoc #-}

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
#if __GLASGOW_HASKELL__ >= 900
pattern RealSrcSpan' r <- RealSrcSpan r _ where
  RealSrcSpan' r = RealSrcSpan r Nothing
#else
pattern RealSrcSpan' r <- RealSrcSpan r where
  RealSrcSpan' r = RealSrcSpan r
#endif
{-# COMPLETE RealSrcSpan', UnhelpfulSpan #-}

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
findParent :: Data a => AnnSpan -> Anns -> a -> Maybe AnnKey
findParent ss as = something (findParentWorker ss as)

-- Note that a parent must also have an annotation.
findParentWorker :: forall a . (Data a)
           => AnnSpan -> Anns -> a -> Maybe AnnKey
findParentWorker oldSS as a
  | con == typeRepTyCon (typeRep (Proxy :: Proxy (GHC.Located GHC.RdrName))) && x == typeRep (Proxy :: Proxy AnnSpan)
      = if ss == oldSS
            && isJust (Map.lookup (AnnKey ss cn) as)
          then Just $ AnnKey ss cn
          else Nothing
  | otherwise = Nothing
  where
    (con, ~[x, _]) = splitTyConApp (typeOf a)
    ss :: AnnSpan
    ss = gmapQi 0 unsafeCoerce a
    cn = gmapQi 1 (CN . show . toConstr) a

getAnnSpan :: forall a. Located a -> AnnSpan
getAnnSpan = srcSpanToAnnSpan . getLoc

srcSpanToAnnSpan :: SrcSpan -> AnnSpan
srcSpanToAnnSpan =
#if __GLASGOW_HASKELL__ >= 900
  \case GHC.RealSrcSpan l _ -> l; _ -> badRealSrcSpan
#else
  id
#endif

annSpanToSrcSpan :: AnnSpan -> SrcSpan
annSpanToSrcSpan =
#if __GLASGOW_HASKELL__ >= 900
  flip GHC.RealSrcSpan Nothing
#else
  id
#endif

-- | Perform the necessary adjustments to annotations when replacing
-- one Located thing with another Located thing.
--
-- For example, this function will ensure the correct relative position and
-- make sure that any trailing semi colons or commas are transferred.
modifyAnnKey
  :: (Data old, Data new, Data mod)
  => mod -> Located old -> Located new -> M (Located new)
modifyAnnKey m e1 e2 = do
  as <- gets fst
  let parentKey = fromMaybe (mkAnnKey e2) (findParent (getAnnSpan e2) as m)
  e2 <$ modify
          ( bimap
              ( recoverBackquotes e1 e2
              . replaceAnnKey (mkAnnKey e1) (mkAnnKey e2) (mkAnnKey e2) parentKey
              )
              (Map.insertWith (++) (mkAnnKey e1) [mkAnnKey e2])
          )

-- | When the template contains a backquoted substitution variable, but the substitute
-- is not backquoted, we must add the corresponding 'GHC.AnnBackQuote's.
--
-- See tests/examples/Backquotes.hs for an example.
recoverBackquotes :: Located old -> Located new -> Anns -> Anns
recoverBackquotes (getAnnSpan -> old) (getAnnSpan -> new) anns
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
toGhcSrcSpan = toGhcSrcSpan' . GHC.mkFastString

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan' :: FastString -> R.SrcSpan -> SrcSpan
toGhcSrcSpan' file R.SrcSpan{..} = mkSrcSpan (f startLine startCol) (f endLine endCol)
  where
    f = mkSrcLoc file

setSrcSpanFile :: FastString -> SrcSpan -> SrcSpan
setSrcSpanFile file s
  | RealSrcLoc' start <- srcSpanStart s
  , RealSrcLoc' end <- srcSpanEnd s
  = let start' = mkSrcLoc file (srcLocLine start) (srcLocCol start)
        end' = mkSrcLoc file (srcLocLine end) (srcLocCol end)
     in mkSrcSpan start' end'
setSrcSpanFile _ s = s

setRealSrcSpanFile :: FastString -> RealSrcSpan -> RealSrcSpan
setRealSrcSpanFile file s = mkRealSrcSpan start' end'
  where
    start = realSrcSpanStart s
    end = realSrcSpanEnd s
    start' = mkRealSrcLoc file (srcLocLine start) (srcLocCol start)
    end' = mkRealSrcLoc file (srcLocLine end) (srcLocCol end)

setAnnSpanFile :: FastString -> AnnSpan -> AnnSpan
setAnnSpanFile =
#if __GLASGOW_HASKELL__ >= 900
  setRealSrcSpanFile
#else
  setSrcSpanFile
#endif
