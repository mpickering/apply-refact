{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module Refact.Apply
  ( applyRefactorings
  , runRefactoring
  , parseExtensions
  ) where

import Data.List
import GHC.LanguageExtensions.Type (Extension(..))
import Refact.Fixity
import Refact.Internal
import Refact.Types

#if __GLASGOW_HASKELL__ <= 806
import DynFlags (FlagSpec(flagSpecFlag, flagSpecName), xFlags)
#else
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (impliedXFlags, readExtension)
#endif

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

#if __GLASGOW_HASKELL__ <= 806
readExtension :: String -> Maybe Extension
readExtension s = flagSpecFlag <$> find ((== s) . flagSpecName) xFlags

-- | Copied from "Language.Haskell.GhclibParserEx.GHC.Driver.Session", in order to
-- support GHC 8.6
impliedXFlags :: [(Extension, Bool, Extension)]
impliedXFlags
-- See Note [Updating flag description in the User's Guide]
  = [ (RankNTypes,                True, ExplicitForAll)
    , (QuantifiedConstraints,     True, ExplicitForAll)
    , (ScopedTypeVariables,       True, ExplicitForAll)
    , (LiberalTypeSynonyms,       True, ExplicitForAll)
    , (ExistentialQuantification, True, ExplicitForAll)
    , (FlexibleInstances,         True, TypeSynonymInstances)
    , (FunctionalDependencies,    True, MultiParamTypeClasses)
    , (MultiParamTypeClasses,     True, ConstrainedClassMethods)  -- c.f. #7854
    , (TypeFamilyDependencies,    True, TypeFamilies)

    , (RebindableSyntax, False, ImplicitPrelude)      -- NB: turn off!

    , (DerivingVia, True, DerivingStrategies)

    , (GADTs,            True, GADTSyntax)
    , (GADTs,            True, MonoLocalBinds)
    , (TypeFamilies,     True, MonoLocalBinds)

    , (TypeFamilies,     True, KindSignatures)  -- Type families use kind signatures
    , (PolyKinds,        True, KindSignatures)  -- Ditto polymorphic kinds

    -- TypeInType is now just a synonym for a couple of other extensions.
    , (TypeInType,       True, DataKinds)
    , (TypeInType,       True, PolyKinds)
    , (TypeInType,       True, KindSignatures)

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (AutoDeriveTypeable, True, DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (TypeFamilies,     True, ExplicitNamespaces)
    , (TypeOperators, True, ExplicitNamespaces)

    , (ImpredicativeTypes,  True, RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (RecordWildCards,     True, DisambiguateRecordFields)

    , (ParallelArrays, True, ParallelListComp)

    , (JavaScriptFFI, True, InterruptibleFFI)

    , (DeriveTraversable, True, DeriveFunctor)
    , (DeriveTraversable, True, DeriveFoldable)

    -- Duplicate record fields require field disambiguation
    , (DuplicateRecordFields, True, DisambiguateRecordFields)

    , (TemplateHaskell, True, TemplateHaskellQuotes)
    , (Strict, True, StrictData)
  ]
#endif
