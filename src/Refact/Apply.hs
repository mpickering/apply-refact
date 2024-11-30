module Refact.Apply
  ( applyRefactorings,
    applyRefactorings',
    runRefactoring,
    parseExtensions,
  )
where

import Control.Monad (unless)
import Data.List (intercalate)
import Refact.Compat (Module)
import Refact.Fixity (applyFixities)
import GHC (DynFlags)
import Refact.Internal
import Refact.Types (Refactoring, SrcSpan)

-- | Apply a set of refactorings as supplied by HLint
applyRefactorings ::
  -- | FilePath to [GHC's libdir](https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#ghc-flag---print-libdir).
  --
  -- It is possible to use @libdir@ from [ghc-paths package](https://hackage.haskell.org/package/ghc-paths), but note
  -- this will make it difficult to provide a binary distribution of your program.
  FilePath ->
  -- | Apply hints relevant to a specific position
  Maybe (Int, Int) ->
  -- | 'Refactoring's to apply. Each inner list corresponds to an HLint
  -- <https://hackage.haskell.org/package/hlint/docs/Language-Haskell-HLint.html#t:Idea Idea>.
  -- An @Idea@ may have more than one 'Refactoring'.
  --
  -- The @Idea@s are sorted in ascending order of starting location, and are applied
  -- in that order. If two @Idea@s start at the same location, the one with the larger
  -- source span comes first. An @Idea@ is filtered out (ignored) if there is an @Idea@
  -- prior to it which has an overlapping source span and is not filtered out.
  [[Refactoring SrcSpan]] ->
  -- | Target file
  FilePath ->
  -- | GHC extensions, e.g., @LambdaCase@, @NoStarIsType@. The list is processed from left
  -- to right. An extension (e.g., @StarIsType@) may be overridden later (e.g., by @NoStarIsType@).
  --
  -- These are in addition to the @LANGUAGE@ pragmas in the target file. When they conflict
  -- with the @LANGUAGE@ pragmas, pragmas win.
  [String] ->
  IO String
applyRefactorings libdir optionsPos inp file exts = do
  let (enabled, disabled, invalid) = parseExtensions exts
  unless (null invalid) . fail $ "Unsupported extensions: " ++ intercalate ", " invalid
  (dfs, m) <-
    either (onError "apply") pure
      =<< parseModuleWithArgs libdir (enabled, disabled) file
  m' <- applyFixities m
  apply dfs optionsPos False ((mempty,) <$> inp) (Just file) Silent m'

-- | Like 'applyRefactorings', but takes a parsed module rather than a file path to parse.
applyRefactorings' ::
  DynFlags ->
  Maybe (Int, Int) ->
  [[Refactoring SrcSpan]] ->
  -- | ghc-exactprint AST annotations. This can be obtained from
  -- 'Language.Haskell.GHC.ExactPrint.Parsers.postParseTransform'.
  -- Anns ->
  -- | Parsed module
  Module ->
  IO String
applyRefactorings' dfs optionsPos inp m0 =
  apply dfs optionsPos False ((mempty,) <$> inp) Nothing Silent m0
