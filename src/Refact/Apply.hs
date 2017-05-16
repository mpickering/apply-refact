{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Refact.Apply
  (
    runRefactoring
  , applyRefactorings

  -- * Support for runPipe in the main process
  , Verbosity(..)
  , rigidLayout
  , removeOverlap
  , refactOptions
  )  where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Print
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Data.Maybe
import Data.List hiding (find)
import Data.Ord

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Data
import Data.Generics.Schemes

import HsExpr as GHC hiding (Stmt)
import HsImpExp
import HsSyn hiding (Pat, Stmt)
import SrcLoc
import qualified GHC hiding (parseModule)
import qualified OccName as GHC
import Data.Generics hiding (GT)

import qualified Data.Map as Map

import System.IO.Unsafe

import Control.Arrow

import Debug.Trace

import Data.Monoid
import Refact.Fixity
import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import Refact.Utils (Stmt, Pat, Name, Decl, M, Expr, Type, FunBind
                    , modifyAnnKey, replaceAnnKey, Import, toGhcSrcSpan)

-- library access to perform the substitutions

refactOptions :: PrintOptions Identity String
refactOptions = stringOptions { epRigidity = RigidLayout }

rigidLayout :: DeltaOptions
rigidLayout = deltaOptions RigidLayout

-- | Apply a set of refactorings as supplied by hlint
applyRefactorings :: Maybe (Int, Int) -> [(String, [Refactoring R.SrcSpan])] -> FilePath -> IO String
applyRefactorings optionsPos inp file = do
  (as, m) <- either (error . show) (uncurry applyFixities)
              <$> parseModuleWithOptions rigidLayout file
  let noOverlapInp = removeOverlap Silent inp
      refacts = (fmap . fmap . fmap) (toGhcSrcSpan file) <$> noOverlapInp

      posFilter (_, rs) =
        case optionsPos of
          Nothing -> True
          Just p  -> any (flip spans p . pos) rs
      filtRefacts = filter posFilter refacts

  -- need a check here to avoid overlap
  (ares, res) <- return . flip evalState 0 $
                          foldM (uncurry runRefactoring) (as, m) (concatMap snd filtRefacts)
  let output = runIdentity $ exactPrintWithOptions refactOptions res ares
  return output

data Verbosity = Silent | Normal | Loud deriving (Eq, Show, Ord)

-- Filters out overlapping ideas, picking the first idea in a set of overlapping ideas.
-- If two ideas start in the exact same place, pick the largest edit.
removeOverlap :: Verbosity -> [(String, [Refactoring R.SrcSpan])] -> [(String, [Refactoring R.SrcSpan])]
removeOverlap verb = dropOverlapping . sortBy f . summarize
  where
    -- We want to consider all Refactorings of a single idea as a unit, so compute a summary
    -- SrcSpan that encompasses all the Refactorings within each idea.
    summarize :: [(String, [Refactoring R.SrcSpan])] -> [(String, (R.SrcSpan, [Refactoring R.SrcSpan]))]
    summarize ideas = [ (s, (foldr1 summary (map pos rs), rs)) | (s, rs) <- ideas, not (null rs) ]

    summary (R.SrcSpan sl1 sc1 el1 ec1)
            (R.SrcSpan sl2 sc2 el2 ec2) =
      let (sl, sc) = case compare sl1 sl2 of
                      LT -> (sl1, sc1)
                      EQ -> (sl1, min sc1 sc2)
                      GT -> (sl2, sc2)
          (el, ec) = case compare el1 el2 of
                      LT -> (el2, ec2)
                      EQ -> (el2, max ec1 ec2)
                      GT -> (el1, ec1)
      in R.SrcSpan sl sc el ec

    -- Order by span start. If starting in same place, order by size.
    f (_,(s1,_)) (_,(s2,_)) =
      comparing startLine s1 s2 <> -- s1 first if it starts on earlier line
      comparing startCol s1 s2 <>  --             or on earlier column
      comparing endLine s2 s1 <>   -- they start in same place, s2 comes
      comparing endCol s2 s1       -- first if it ends later
      -- else, completely same span, so s1 will be first

    dropOverlapping [] = []
    dropOverlapping (p:ps) = go p ps
    go (s,(_,rs)) [] = [(s,rs)]
    go p@(s,(_,rs)) (x:xs)
      | p `overlaps` x = (if verb > Silent
                          then trace ("Ignoring " ++ show (snd (snd x)) ++ " due to overlap.")
                          else id) go p xs
      | otherwise = (s,rs) : go x xs
    -- for overlaps, we know s1 always starts <= s2, due to our sort
    overlaps (_,(s1,_)) (_,(s2,_)) =
      case compare (startLine s2) (endLine s1) of
        LT -> True
        EQ -> startCol s2 <= endCol s1
        GT -> False

-- ---------------------------------------------------------------------

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

-- Specialised parsers


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
      case unLoc (GHC.mg_alts fun_matches) of
           [x] -> Right (as, x)
           _   -> Left (l, "Not a single match")
    Right (_, GHC.L l _) -> Left (l, "Not a funbind")
    Left e -> Left e

-- Substitute variables into templates
-- Finds places in the templates where we need to insert variables.

substTransform :: (Data a, Data b) => b -> [(String, GHC.SrcSpan)] -> a -> M a
substTransform m ss = everywhereM (mkM (typeSub m ss)
                                    `extM` identSub m ss
                                    `extM` patSub m ss
                                    `extM` stmtSub m ss
                                    `extM` exprSub m ss
                                    )

stmtSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Stmt -> M Stmt
stmtSub m subs old@(GHC.L _ (BodyStmt (GHC.L _ (HsVar (L _ name))) _ _ _) ) =
  resolveRdrName m (findStmt m) old subs name
stmtSub _ _ e = return e

patSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Pat -> M Pat
patSub m subs old@(GHC.L _ (VarPat (L _ name))) =
  resolveRdrName m (findPat m) old subs name
patSub _ _ e = return e

typeSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Type -> M Type
typeSub m subs old@(GHC.L _ (HsTyVar _ (L _ name))) =
  resolveRdrName m (findType m) old subs name
typeSub _ _ e = return e

exprSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Expr -> M Expr
exprSub m subs old@(GHC.L _ (HsVar (L _ name))) =
  resolveRdrName m (findExpr m) old subs name
exprSub _ _ e = return e


-- Used for Monad10, Monad11 tests.
-- The issue being that in one case the information is attached to a VarPat
-- but we need to move the annotations onto the actual name
--
-- This looks convoluted but we can't match directly on a located name as
-- it is not specific enough. Instead we match on some bigger context which
-- is contains the located name we want to replace.
identSub :: Data a => a -> [(String, GHC.SrcSpan)] -> FunBind -> M FunBind
identSub m subs old@(GHC.FunRhs (GHC.L _ name) _) =
  resolveRdrName' subst (findName m) old subs name
  where
    subst :: FunBind -> Name -> M FunBind
    subst (GHC.FunRhs n b) new = do
      let fakeExpr = GHC.L (getLoc new) (GHC.VarPat new)
      -- Low level version as we need to combine the annotation information
      -- from the template RdrName and the original VarPat.
      modify (\r -> replaceAnnKey r (mkAnnKey n) (mkAnnKey fakeExpr) (mkAnnKey new) (mkAnnKey fakeExpr))
      return $ GHC.FunRhs new b
    subst o _ = return o
identSub _ _ e = return e



-- g is usually modifyAnnKey
-- f is usually a function which checks the locations are equal
resolveRdrName' :: (a -> b -> M a)  -- How to combine the value to insert and the replaced value
               -> (SrcSpan -> b)    -- How to find the new value, when given the location it is in
               -> a                 -- The old thing which we are going to possibly replace
               -> [(String, GHC.SrcSpan)] -- Substs
               -> GHC.RdrName       -- The name of the position in the template
                                    --we are replacing into
               -> M a
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


-- Substitute the template into the original AST.
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

findName :: Data a => a -> SrcSpan -> Name
findName = findGen "name"

findLargestExpression :: SrcSpan -> GHC.Located ast
                      -> Maybe (GHC.Located ast)
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
