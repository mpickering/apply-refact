{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Refact.Apply  where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Data.Data
import Data.Generics.Schemes

import HsExpr as GHC hiding (Stmt)
import qualified HsBinds as GHC
import qualified HsDecls as GHC
import HsImpExp
import FastString
import qualified Module as GHC
import HsSyn hiding (Pat, Stmt)
import SrcLoc
import qualified SrcLoc as GHC
import qualified RdrName as GHC
import qualified OccName as GHC
import qualified Outputable as GHC
import Data.Generics
import Control.Monad.State
import Data.Tuple

import qualified Data.Map as Map

import System.IO.Unsafe

import Control.Arrow
import Control.Monad.State

import Data.Maybe

import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import Refact.Utils (Module, Stmt, Pat, Name, Decl, M, Expr, Type
                    , mergeAnns, modifyAnnKey, replaceAnnKey, Import)

import Debug.Trace


-- Perform the substitutions

getSeed :: State Int Int
getSeed = get <* modify (+1)

runRefactoring :: Anns -> Module -> Refactoring GHC.SrcSpan -> State Int (Anns, Module)
runRefactoring as m r@Replace{}  = do
  seed <- getSeed
  return $ case rtype r of
    Expr -> replaceWorker as m parseExpr (doGenReplacement m) seed r
    Decl -> replaceWorker as m parseDecl (doGenReplacement m) seed r
    Type -> replaceWorker as m parseType (doGenReplacement m) seed r
    Pattern -> replaceWorker as m parsePattern (doGenReplacement m) seed r
    Stmt -> replaceWorker as m parseStmt (doGenReplacement m) seed r
    Bind -> replaceWorker as m parseBind (doGenReplacement m) seed r
    R.Match ->  replaceWorker as m parseMatch (doGenReplacement m) seed r
    ModuleName -> replaceWorker as m (parseModuleName (pos r))(doGenReplacement m) seed r
    Import -> replaceWorker as m parseImport (doGenReplacement m) seed r

runRefactoring as m ModifyComment{..} =
    return $ (Map.map go as, m)
    where
      go a@(Ann{ annPriorComments, annsDP }) =
        a { annsDP = map changeComment annsDP
          , annPriorComments = map (first change) annPriorComments }
      changeComment (AnnComment d, dp) = (AnnComment (change d), dp)
      changeComment e = e
      change old@Comment{..}= if ss2pos commentIdentifier == ss2pos pos
                                          then old { commentContents = newComment}
                                          else old
runRefactoring as m Delete{rtype, pos} = do
  let f = case rtype of
            Stmt -> doDeleteStmt ((/= pos) . getLoc)
            Import -> doDeleteImport ((/= pos) . getLoc)
            _ -> id
  return $ (as, f m)
  {-
runRefactoring as m Rename{nameSubts} = (as, m)
  --(as, doRename nameSubts m)
 -}
runRefactoring as m InsertComment{..} =
  let exprkey = mkAnnKey (findDecl m pos) in
  return $ (insertComment exprkey newComment as, m)
runRefactoring as m RemoveAsKeyword{..} =
  return (as, removeAsKeyword m)
  where
    removeAsKeyword = everywhere (mkT go)
    go :: LImportDecl GHC.RdrName -> LImportDecl GHC.RdrName
    go imp@(GHC.L l i)  | l == pos = GHC.L l (i { ideclAs = Nothing })
                    | otherwise =  imp



parseModuleName :: GHC.SrcSpan -> Parser (GHC.Located GHC.ModuleName)
parseModuleName ss _ (mkFastString -> fname) s =
  let newMN =  GHC.L ss (GHC.mkModuleName s)
      newAnns = relativiseApiAnns newMN (Map.empty, Map.empty)
  in return (newAnns, newMN)
parseBind :: Parser (GHC.LHsBind GHC.RdrName)
parseBind dyn fname s =
  case parseDecl dyn fname s of
    -- Safe as we add no annotations to the ValD
    Right (as, GHC.L l (GHC.ValD b)) -> Right (as, GHC.L l b)
    Right (_, GHC.L l _) -> Left (l, "Not a HsBind")
    Left e -> Left e
parseMatch :: Parser (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
parseMatch dyn fname s =
  case parseBind dyn fname s of
    Right (as, GHC.L l GHC.FunBind{fun_matches}) ->
      case GHC.mg_alts fun_matches of
           [x] -> Right (as, x)
           _   -> Left (l, "Not a single match")
    Right (as, GHC.L l _) -> Left (l, "Not a funbind")
    Left e -> Left e

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
  resolveRdrName m (findStmt m) old subs name
stmtSub _ _ e = return e

patSub :: Module -> [(String, GHC.SrcSpan)] -> Pat -> M Pat
patSub m subs old@(GHC.L _ (VarPat name)) =
  resolveRdrName m (findPat m) old subs name
patSub _ _ e = return e

typeSub :: Module -> [(String, GHC.SrcSpan)] -> Type -> M Type
typeSub m subs old@(GHC.L _ (HsTyVar name)) =
  resolveRdrName m (findType m) old subs name
typeSub _ _ e = return e

exprSub :: Module -> [(String, GHC.SrcSpan)] -> Expr -> M Expr
exprSub m subs old@(GHC.L _ (HsVar name)) =
  resolveRdrName m (findExpr m) old subs name
exprSub _ _ e = return e

identSub :: Module -> [(String, GHC.SrcSpan)] -> Name -> M Name
identSub m subs old@(GHC.L _ name) =
  resolveRdrName' subst (findName m) old subs name
  where
    subst :: Name -> (Name, Pat) -> M Name
    subst (mkAnnKey -> oldkey) (n, p)
      = n <$ modify (\r -> replaceAnnKey r oldkey (mkAnnKey p) (mkAnnKey n) (mkAnnKey p))

resolveRdrName' ::
                  (a -> b -> M a)  -> (SrcSpan -> b) -> a
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
               => Module
               -> (SrcSpan -> Located old)
               -> Located old
               -> [(String, SrcSpan)]
               -> GHC.RdrName
               -> M (Located old)
resolveRdrName m = resolveRdrName' (modifyAnnKey m)

insertComment :: AnnKey -> String
              -> Map.Map AnnKey Annotation
              -> Map.Map AnnKey Annotation
insertComment k s as =
  let comment = Comment s GHC.noSrcSpan Nothing in
  Map.adjust (\a@Ann{..} -> a { annPriorComments = annPriorComments ++ [(comment, DP (1,0))]
                          , annEntryDelta = DP (1,0) }) k as


doGenReplacement :: Data ast
              => Module
              -> (GHC.Located ast -> Bool)
              -> GHC.Located ast
              -> GHC.Located ast
              -> State (Anns, Bool) (GHC.Located ast)
doGenReplacement m p new old =
  if p old then do
                  s <- get
                  let (v, st) = runState (modifyAnnKey m old new) (fst s)
                  modify (\_ -> (st, True))
                  return v
           else return old

type Repl a = (GHC.Located a -> Bool) -> GHC.Located a -> GHC.Located a -> State (Anns, Bool) (GHC.Located a)

replaceWorker :: (Annotate a) => Anns -> Module
              -> Parser (GHC.Located a) -> Repl a -> Int
              -> Refactoring GHC.SrcSpan -> (Anns, Module)
replaceWorker as m parser r seed Replace{..} =
  let replExprLocation = pos
      uniqueName = "template" ++ show seed
      p s = unsafePerformIO (withDynFlags (\d -> parser d uniqueName s))
      (relat, template) = case p orig of
                              Right xs -> xs
                              Left err -> error (show err)
      (newExpr, newAnns) = (runState (substTransform m subts template) (mergeAnns as relat))
      replacementPred (GHC.L l _) = l == replExprLocation
      transformation = (everywhereM (mkM (r replacementPred newExpr)))
   in case runState (transformation m) (newAnns, False) of
        (finalM, (finalAs, True)) -> trace "Success" (finalAs, finalM)
        -- Failed to find a replacment so don't make any changes
        _ -> trace "Failure" (as, m)
replaceWorker as m _ _ _ _  = (as, m)



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
       GHC.L l _ -> error $ "Not var pat: " ++ showGhc l


findLargestExpression :: SrcSpan -> GHC.Located ast -> Maybe (GHC.Located ast)
findLargestExpression ss e@(GHC.L l _) =
  if l == ss
    then Just e
    else Nothing

-- Deletion from a list

doDeleteStmt :: Data a => (Stmt -> Bool) -> a -> a
doDeleteStmt p = everywhere (mkT (filter p))

doDeleteImport :: Data a => (Import -> Bool) -> a -> a
doDeleteImport p = everywhere (mkT (filter p))

-- Renaming

doRename :: [(String, String)] -> Module -> Module
doRename ss = everywhere (mkT rename)
  where
    rename :: GHC.OccName -> GHC.OccName
    rename v = GHC.mkOccName n s'
      where
          (s, n) = (GHC.occNameString v, GHC.occNameSpace v)
          s' = fromMaybe s (lookup s ss)
