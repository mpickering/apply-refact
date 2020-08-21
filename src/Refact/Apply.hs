{-# LANGUAGE TupleSections #-}

module Refact.Apply
  ( runRefactoring
  , applyRefactorings
  ) where

import Language.Haskell.GHC.ExactPrint.Parsers (parseModuleWithOptions)
import Refact.Fixity
import Refact.Internal
import Refact.Types

-- | Apply a set of refactorings as supplied by hlint
applyRefactorings
  :: Maybe (Int, Int)
  -- ^ Apply hints relevant to a specific position
  -> [[Refactoring SrcSpan]]
  -- ^ 'Refactoring's to apply. Each inner list corresponds to an HLint
  -- <https://hackage.haskell.org/package/hlint/docs/Language-Haskell-HLint.html#t:Idea Idea>.
  -- An @Idea@ may have more than one 'Refactoring'.
  --
  -- The @Idea@s are sorted in ascending order of starting location, and are applied
  -- in that order. If two @Idea@s start at the same location, the one with the larger
  -- source span comes first. An @Idea@ is filtered out (ignored) if there is an @Idea@
  -- prior to it which has an overlapping source span and is not filtered out.
  -> FilePath
  -- ^ Target file
  -> IO String
applyRefactorings optionsPos inp file = do
  (as, m) <- either (onError "apply") (uncurry applyFixities)
              <$> parseModuleWithOptions rigidLayout file
  apply optionsPos False ((mempty,) <$> inp) file Silent as m
