{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Refact.Apply (runRefactoring)  where

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
import qualified Module as GHC
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
import qualified Refact.Types as R
import Refact.Utils (Stmt, Pat, Name, Decl, M, Expr, Type
                    , modifyAnnKey, replaceAnnKey, Import)


-- Perform the substitutions

getSeed :: State Int Int
getSeed = get <* modify (+1)

-- | Peform a @Refactoring@.
runRefactoring :: Data a => Anns -> a -> Refactoring GHC.SrcSpan -> State Int (Anns, a)
runRefactoring as m r@Replace{}  = do
  seed <- getSeed
  return $ case rtype r of
    Expr -> replaceWorker as m parseExpr seed r
    Decl -> replaceWorker as m parseDecl seed r
    Type -> replaceWorker as m parseType seed r
    Pattern -> replaceWorker as m parsePattern seed r
    Stmt -> replaceWorker as m parseStmt seed r
    Bind -> replaceWorker as m parseBind seed r
    R.Match ->  replaceWorker as m parseMatch seed r
    ModuleName -> replaceWorker as m (parseModuleName (pos r)) seed r
    Import -> replaceWorker as m parseImport seed r

runRefactoring as m ModifyComment{..} =
    return (Map.map go as, m)
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
  return (as, f m)
  {-
runRefactoring as m Rename{nameSubts} = (as, m)
  --(as, doRename nameSubts m)
 -}
runRefactoring as m InsertComment{..} =
  let exprkey = mkAnnKey (findDecl m pos) in
  return (insertComment exprkey newComment as, m)
runRefactoring as m RemoveAsKeyword{..} =
  return (as, removeAsKeyword m)
  where
    removeAsKeyword = everywhere (mkT go)
    go :: LImportDecl GHC.RdrName -> LImportDecl GHC.RdrName
    go imp@(GHC.L l i)  | l == pos = GHC.L l (i { ideclAs = Nothing })
                    | otherwise =  imp



parseModuleName :: GHC.SrcSpan -> Parser (GHC.Located GHC.ModuleName)
parseModuleName ss _ _ s =
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
    Right (_, GHC.L l _) -> Left (l, "Not a funbind")
    Left e -> Left e

-- Substitute variables into templates

substTransform :: (Data a, Data b) => b -> [(String, GHC.SrcSpan)] -> a -> M a
substTransform m ss = everywhereM (mkM (exprSub m ss)
                                    `extM` typeSub m ss
                                    `extM` patSub m ss
                                    `extM` stmtSub m ss
                                    `extM` identSub m ss
                                    )

stmtSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Stmt -> M Stmt
stmtSub m subs old@(GHC.L _ (BodyStmt (GHC.L _ (HsVar name)) _ _ _) ) =
  resolveRdrName m (findStmt m) old subs name
stmtSub _ _ e = return e

patSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Pat -> M Pat
patSub m subs old@(GHC.L _ (VarPat name)) =
  resolveRdrName m (findPat m) old subs name
patSub _ _ e = return e

typeSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Type -> M Type
typeSub m subs old@(GHC.L _ (HsTyVar name)) =
  resolveRdrName m (findType m) old subs name
typeSub _ _ e = return e

exprSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Expr -> M Expr
exprSub m subs old@(GHC.L _ (HsVar name)) =
  resolveRdrName m (findExpr m) old subs name
exprSub _ _ e = return e

identSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Name -> M Name
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

resolveRdrName :: (Data old, Data a)
               => a
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


doGenReplacement :: (Data ast, Data a)
              => a
              -> (GHC.Located ast -> Bool)
              -> GHC.Located ast
              -> GHC.Located ast
              -> State (Anns, Bool) (GHC.Located ast)
doGenReplacement m p new old =
  if p old then do
                  s <- get
                  let (v, st) = runState (modifyAnnKey m old new) (fst s)
                  modify (const (st, True))
                  return v
           else return old

replaceWorker :: (Annotate a, Data mod) => Anns -> mod
              -> Parser (GHC.Located a) -> Int
              -> Refactoring GHC.SrcSpan -> (Anns, mod)
replaceWorker as m parser seed Replace{..} =
  let replExprLocation = pos
      uniqueName = "template" ++ show seed
      p s = unsafePerformIO (withDynFlags (\d -> parser d uniqueName s))
      (relat, template) = case p orig of
                              Right xs -> xs
                              Left err -> error (show err)
      (newExpr, newAnns) = runState (substTransform m subts template) (mergeAnns as relat)
      replacementPred (GHC.L l _) = l == replExprLocation
      transformation = everywhereM (mkM (doGenReplacement m replacementPred newExpr))
   in case runState (transformation m) (newAnns, False) of
        (finalM, (finalAs, True)) -> (finalAs, finalM)
        -- Failed to find a replacment so don't make any changes
        _ -> (as, m)
replaceWorker as m _ _ _  = (as, m)



-- Find the largest expression with a given SrcSpan
findGen :: forall ast a . (Data ast, Data a) => String -> a -> SrcSpan -> GHC.Located ast
findGen s m ss = fromMaybe (error (s ++ " " ++ showGhc ss)) (doTrans m)
  where
    doTrans :: a -> Maybe (GHC.Located ast)
    doTrans = something (mkQ Nothing (findLargestExpression ss))

findExpr :: Data a => a -> SrcSpan -> Expr
findExpr = findGen "expr"

findPat :: Data a => a -> SrcSpan -> Pat
findPat = findGen "pat"

findType :: Data a => a -> SrcSpan -> Type
findType = findGen "type"

findDecl :: Data a => a -> SrcSpan -> Decl
findDecl = findGen "decl"

findStmt :: Data a => a -> SrcSpan -> Stmt
findStmt = findGen "stmt"

findName :: Data a => a -> SrcSpan -> (Name, Pat)
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

{-
-- Renaming

doRename :: [(String, String)] -> Module -> Module
doRename ss = everywhere (mkT rename)
  where
    rename :: GHC.OccName -> GHC.OccName
    rename v = GHC.mkOccName n s'
      where
          (s, n) = (GHC.occNameString v, GHC.occNameSpace v)
          s' = fromMaybe s (lookup s ss)
-}
