{-# LANGUAGE RecordWildCards #-}
module Test where

import Test.Tasty
import Test.Tasty.Golden
import System.Directory
import System.FilePath

import Options.Applicative

import Main hiding (main)

import System.IO.Silently
import System.IO

import Debug.Trace


main = do
  setCurrentDirectory "tests/examples"
  defaultMain =<< mkTests <$> findTests

findTests :: IO [FilePath]
findTests = do
  files <- getDirectoryContents "./"
  return $ (filter ((== ".hs") . takeExtension ) files)


mkTests :: [FilePath] -> TestTree
mkTests files = testGroup "Unit tests" (map mkTest files)
  where
    mkTest :: FilePath -> TestTree
    mkTest fp =
      let outfile = fp <.> "out"
          topts = Options
                  { optionsTarget       = Just fp
                  , optionsInplace       = False
                  , optionsOutput        = Just outfile
                  , optionsSuggestions   = False
                  , optionsHlintOptions  = ""
                  , optionsVerbosity     = Silent
                  , optionsStep          = False
                  , optionsRoundtrip     = False
                  , optionsDebug         = False
                  }
          action = hSilence [stderr] $ runPipe topts fp
          diffCmd = \ref new -> ["diff", "-u", ref, new]
      in goldenVsFileDiff fp diffCmd (fp <.> "expected") outfile action



