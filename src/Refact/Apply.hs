{-# LANGUAGE TupleSections #-}

module Refact.Apply
  ( applyRefactorings
  , runRefactoring
  , parseExtensions
  ) where

import Data.List
import GHC.LanguageExtensions.Type (Extension(..))
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (impliedXFlags, readExtension)
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
  -> ([Extension], [Extension])
  -- ^ Enabled and disabled extensions. These are in addition to the @LANGUAGE@ pragmas
  -- in the target file. When they conflict with the @LANGUAGE@ pragmas, pragmas win.
  -> IO String
applyRefactorings optionsPos inp file exts = do
  (as, m) <- either (onError "apply") (uncurry applyFixities)
              =<< parseModuleWithArgs exts file
  apply optionsPos False ((mempty,) <$> inp) file Silent as m

-- | Parse the input into (enabled extensions, disabled extensions, invalid input).
-- Implied extensions are automatically added. For example, @FunctionalDependencies@
-- implies @MultiParamTypeClasses@, and @RebindableSyntax@ implies @NoImplicitPrelude@.
--
-- The input is processed from left to right. An extension (e.g., @StarIsType@)
-- may be overridden later (e.g., by @NoStarIsType@).
--
-- Extensions that appear earlier in the input will appear later in the output.
-- Implied extensions appear in the end. If an extension occurs multiple times in the input,
-- the last one is used.
--
-- >>> parseExtensions ["GADTs", "RebindableSyntax", "StarIsType", "GADTs", "InvalidExtension", "NoStarIsType"]
-- ([GADTs, RebindableSyntax, GADTSyntax, MonoLocalBinds], [StarIsType, ImplicitPrelude], ["InvalidExtension"])
parseExtensions :: [String] -> ([Extension], [Extension], [String])
parseExtensions = addImplied . foldl' f mempty
  where
    f :: ([Extension], [Extension], [String]) -> String -> ([Extension], [Extension], [String])
    f (ys, ns, is) ('N' : 'o' : s) | Just ext <- readExtension s =
      (delete ext ys, ext : delete ext ns, is)
    f (ys, ns, is) s | Just ext <- readExtension s =
      (ext : delete ext ys, delete ext ns, is)
    f (ys, ns, is) s = (ys, ns, s : is)

    addImplied :: ([Extension], [Extension], [String]) -> ([Extension], [Extension], [String])
    addImplied (ys, ns, is) = (ys ++ impliedOn, ns ++ impliedOff, is)
      where
        impliedOn = [b | ext <- ys, (a, True, b) <- impliedXFlags, a == ext]
        impliedOff = [b | ext <- ys, (a, False, b) <- impliedXFlags, a == ext]
