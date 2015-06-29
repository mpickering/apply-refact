{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Refact.Perform (runRefactoring) where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Data.Data
import Data.Generics.Schemes

import HsExpr as GHC hiding (Stmt)
import HsSyn hiding (Pat, Stmt)
import SrcLoc
import qualified SrcLoc as GHC
import qualified RdrName as GHC
import qualified OccName as GHC
import Data.Generics
import Control.Monad.State

import qualified Data.Map as Map

import System.IO.Unsafe

import Control.Arrow

import Data.Maybe

import Refact.Types hiding (SrcSpan)
import Refact.Utils (Module, Stmt, Pat, Name, Decl, M, Expr, Type
                    , mergeAnns, modifyAnnKey, replaceAnnKey)

-- Perform the substitutions

runRefactoring :: Anns -> Module -> Refactoring GHC.SrcSpan -> (Anns, Module)
runRefactoring as m r@Replace{}  =
  case rtype r of
    Expr -> replaceWorker as m parseExpr doGenReplacement r
    Decl -> replaceWorker as m parseDecl doGenReplacement r
    Type -> replaceWorker as m parseType doGenReplacement r
    Pattern -> replaceWorker as m parsePattern doGenReplacement r
    Stmt -> replaceWorker as m parseStmt doGenReplacement r
runRefactoring (as,sk) m ModifyComment{..} =
    ((Map.map go as, sk), m)
    where
      go a@(Ann{ annPriorComments, annsDP }) =
        a { annsDP = map changeComment annsDP
          , annPriorComments = map (first change) annPriorComments }
      changeComment (AnnComment d, dp) = (AnnComment (change d), dp)
      changeComment e = e
      change old@Comment{..}= if ss2pos commentIdentifier == ss2pos commentLocation
                                          then old { commentContents = newComment}
                                          else old
runRefactoring as m Delete{position} =
  (as, doDelete ((/= position) . getLoc) m)
runRefactoring as m Rename{nameSubts} =
  (as, doRename nameSubts m)
runRefactoring as m InsertComment{..} =
  let expr = mkAnnKey (findDecl m commentCarrier) in
  (first (insertComment expr newComment) as, m)



-- Substitute variables into templates

substTransform :: Data a => Module -> [(String, GHC.SrcSpan)] -> a -> M a
substTransform m ss = everywhereM (mkM (exprSub m ss)
                                    `extM` typeSub m ss
                                    `extM` patSub m ss
                                    `extM` stmtSub m ss
                                    `extM` identSub m ss
                                    )

stmtSub :: Module -> [(String, GHC.SrcSpan)] -> Stmt -> M Stmt
stmtSub m subs old@(GHC.L _ (BodyStmt (GHC.L _ (HsVar name)) _ _ _) ) =
  resolveRdrName (findStmt m) old subs name
stmtSub _ _ e = return e

patSub :: Module -> [(String, GHC.SrcSpan)] -> Pat -> M Pat
patSub m subs old@(GHC.L _ (VarPat name)) =
  resolveRdrName (findPat m) old subs name
patSub _ _ e = return e

typeSub :: Module -> [(String, GHC.SrcSpan)] -> Type -> M Type
typeSub m subs old@(GHC.L _ (HsTyVar name)) =
  resolveRdrName (findType m) old subs name
typeSub _ _ e = return e

exprSub :: Module -> [(String, GHC.SrcSpan)] -> Expr -> M Expr
exprSub m subs old@(GHC.L _ (HsVar name)) =
  resolveRdrName (findExpr m) old subs name
exprSub _ _ e = return e

identSub :: Module -> [(String, GHC.SrcSpan)] -> Name -> M Name
identSub m subs old@(GHC.L _ name) =
  resolveRdrName' subst (findName m) old subs name
  where
    subst :: Name -> (Name, Pat) -> M Name
    subst (mkAnnKey -> oldkey) (n, p)
      = n <$ modify (\r -> replaceAnnKey r oldkey (mkAnnKey p) (mkAnnKey n))

resolveRdrName' ::
                  (a -> b -> M a)  ->  (SrcSpan -> b) -> a
               -> [(String, GHC.SrcSpan)] -> GHC.RdrName -> M a
resolveRdrName' g f old subs name =
  case name of
    -- Todo: this should replace anns as well?
    GHC.Unqual (GHC.occNameString . GHC.occName -> oname)
      -> case lookup oname subs of
              Just (f -> new) -> g old new
              Nothing -> return old
    _ -> return old

resolveRdrName :: Data old
               => (SrcSpan -> Located old)
               -> Located old
               -> [(String, SrcSpan)]
               -> GHC.RdrName
               -> M (Located old)
resolveRdrName = resolveRdrName' modifyAnnKey

insertComment :: AnnKey -> String
              -> Map.Map AnnKey Annotation
              -> Map.Map AnnKey Annotation
insertComment k s as =
  let comment = Comment (DP (0, length s)) s GHC.noSrcSpan Nothing in
  Map.adjust (\a@Ann{..} -> a { annPriorComments = annPriorComments ++ [(comment, DP (1,0))]
                          , annEntryDelta = DP (1,0) }) k as


doGenReplacement :: Data ast
              => (GHC.Located ast -> Bool)
              -> GHC.Located ast
              -> GHC.Located ast
              -> M (GHC.Located ast)
doGenReplacement p new old =
  if p old then modifyAnnKey old new else return old

type Repl a = (GHC.Located a -> Bool) -> GHC.Located a -> GHC.Located a -> M (GHC.Located a)

replaceWorker :: (Annotate a) => Anns -> Module
              -> Parser (GHC.Located a) -> Repl a
              -> Refactoring GHC.SrcSpan -> (Anns, Module)
replaceWorker as m p r Replace{..} =
  let replExprLocation = expr
      (newanns, template) = case unsafePerformIO $ p orig of
                              Right xs -> xs
                              Left err -> error (show err)
      relat = relativiseApiAnns template newanns
      (newExpr, newAnns) = runState (substTransform m subts template) (mergeAnns as relat)
      replacementPred (GHC.L l _) = l == replExprLocation
      transformation = everywhereM (mkM (r replacementPred newExpr))
      (final, finalanns) = runState (transformation m) newAnns
   in (finalanns, final)
replaceWorker as m _ _ _ = (as, m)


-- Find the largest expression with a given SrcSpan
findGen :: forall ast . Data ast => String -> Module -> SrcSpan -> GHC.Located ast
findGen s m ss = fromMaybe (error (s ++ " " ++ showGhc ss)) (doTrans m)
  where
    doTrans :: Module -> Maybe (GHC.Located ast)
    doTrans = something (mkQ Nothing (findLargestExpression ss))

findExpr :: Module -> SrcSpan -> Expr
findExpr = findGen "expr"

findPat :: Module -> SrcSpan -> Pat
findPat = findGen "pat"

findType :: Module -> SrcSpan -> Type
findType = findGen "type"

findDecl :: Module -> SrcSpan -> Decl
findDecl = findGen "decl"

findStmt :: Module -> SrcSpan -> Stmt
findStmt = findGen "stmt"

findName :: Module -> SrcSpan -> (Name, Pat)
findName m ss =
  case findPat m ss of
       p@(GHC.L l (VarPat n)) -> (GHC.L l n, p)
       _ -> error "Not var pat"


findLargestExpression :: SrcSpan -> GHC.Located ast -> Maybe (GHC.Located ast)
findLargestExpression ss e@(GHC.L l _) =
  if l == ss
    then Just e
    else Nothing

-- Deletion from a list

deleteFromList :: (Stmt -> Bool) -> [Stmt] -> [Stmt]
deleteFromList = filter

doDelete :: Data a => (Stmt -> Bool) -> a -> a
doDelete p = everywhere (mkT (deleteFromList p))

-- Renaming

doRename :: [(String, String)] -> Module -> Module
doRename ss = everywhere (mkT rename)
  where
    rename :: GHC.OccName -> GHC.OccName
    rename v = GHC.mkOccName n s'
      where
          (s, n) = (GHC.occNameString v, GHC.occNameSpace v)
          s' = fromMaybe s (lookup s ss)
