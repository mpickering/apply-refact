{-# LANGUAGE ExplicitNamespaces #-}

module Refact.Apply
  ( runRefactoring
  , applyRefactorings

  -- * Support for runPipe in the main process
  , Verbosity(..)
  , rigidLayout
  , removeOverlap
  , refactOptions
  , type Errors
  , onError
  , mkErr
  ) where

import Refact.Internal
