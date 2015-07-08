{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections  #-}
module Refact.Utils ( -- * Synonyms
                      Module
                    , Stmt
                    , Expr
                    , Decl
                    , Name
                    , Pat
                    , Type
                    -- * Monad
                    , M
                    -- * Utility

                    , mergeAnns
                    , modifyAnnKey
                    , replaceAnnKey
                    , toGhcSrcSpan

                    ) where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Transform (mergeAnns)

import Data.Data
import HsExpr as GHC hiding (Stmt)
import SrcLoc
import qualified SrcLoc as GHC
import qualified RdrName as GHC
import qualified ApiAnnotation as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import Control.Monad.State

import qualified Data.Map as Map


import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)



-- Types
--
type M a = State Anns a

type Module = (GHC.Located (GHC.HsModule GHC.RdrName))

type Expr = GHC.Located (GHC.HsExpr GHC.RdrName)

type Type = GHC.Located (GHC.HsType GHC.RdrName)

type Decl = GHC.Located (GHC.HsDecl GHC.RdrName)

type Pat = GHC.LPat GHC.RdrName

type Name = GHC.Located GHC.RdrName

type Stmt = ExprLStmt GHC.RdrName


-- | Replaces an old expression with a new expression
replace :: AnnKey -> AnnKey -> AnnKey -> Anns -> Maybe Anns
replace old new inp as = do
  let anns = getKeywordDeltas as
  oldan <- Map.lookup old anns
  newan <- Map.lookup new anns
  return $ modifyKeywordDeltas (Map.delete old . Map.insert inp (combine oldan newan)) as

combine :: Annotation -> Annotation -> Annotation
combine oldann newann =
  Ann (annEntryDelta oldann) (annDelta oldann) (annTrueEntryDelta oldann)
      (annPriorComments oldann ++ annPriorComments newann) (annsDP newann ++ extraComma (annsDP oldann)) (annSortKey newann) (annCapturedSpan newann)
  where
    extraComma [] = []
    extraComma (last -> x) = case fst x of
                              G GHC.AnnComma -> [x]
                              AnnSemiSep -> [x]
                              G GHC.AnnSemi -> [x]
                              _ -> []


{-
-- | Shift the first output annotation into the correct place
moveAnns :: [(KeywordId, DeltaPos)] -> [(KeywordId, DeltaPos)] -> [(KeywordId, DeltaPos)]
moveAnns [] xs        = xs
moveAnns _  []        = []
moveAnns ((_, dp): _) ((kw, _):xs) = (kw,dp) : xs
-}

-- | Perform the necessary adjustments to annotations when replacing
-- one Located thing with another Located thing.
--
-- For example, this function will ensure the correct relative position and
-- make sure that any trailing semi colons or commas are transferred.
modifyAnnKey :: (Data old, Data new) => Located old -> Located new -> M (Located new)
modifyAnnKey e1 e2 = e2 <$ modify (\m -> replaceAnnKey m (mkAnnKey e1) (mkAnnKey e2) (mkAnnKey e2))


-- | Lower level version of @modifyAnnKey@
replaceAnnKey ::
  Anns -> AnnKey -> AnnKey -> AnnKey -> Anns
replaceAnnKey a old new inp =
  case replace old new inp a  of
    Nothing -> a
    Just a' -> a'

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan :: FilePath -> R.SrcSpan -> SrcSpan
toGhcSrcSpan file R.SrcSpan{..} = mkSrcSpan (f start) (f end)
  where
    f (x,y) = mkSrcLoc (GHC.mkFastString file) x y
