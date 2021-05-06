module Refact.Apply
  ( applyRefactorings,
    applyRefactorings',
    runRefactoring,
    parseExtensions,
  )
where

import Control.Monad (unless)
import Data.List (intercalate)
import Language.Haskell.GHC.ExactPrint.Types (Anns)
import Refact.Compat (Module)
import Refact.Fixity (applyFixities)
import Refact.Internal
import Refact.Types (Refactoring, SrcSpan)

-- | Apply a set of refactorings as supplied by HLint
applyRefactorings ::
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
applyRefactorings optionsPos inp file exts = do
  let (enabled, disabled, invalid) = parseExtensions exts
  unless (null invalid) . fail $ "Unsupported extensions: " ++ intercalate ", " invalid
  (as, m) <-
    either (onError "apply") (uncurry applyFixities)
      =<< parseModuleWithArgs (enabled, disabled) file
  apply optionsPos False ((mempty,) <$> inp) (Just file) Silent as m

-- | Like 'applyRefactorings', but takes a parsed module rather than a file path to parse.
applyRefactorings' ::
  Maybe (Int, Int) ->
  [[Refactoring SrcSpan]] ->
  -- | ghc-exactprint AST annotations. This can be obtained from
  -- 'Language.Haskell.GHC.ExactPrint.Parsers.postParseTransform'.
  Anns ->
  -- | Parsed module
  Module ->
  IO String
applyRefactorings' optionsPos inp = apply optionsPos False ((mempty,) <$> inp) Nothing Silent
