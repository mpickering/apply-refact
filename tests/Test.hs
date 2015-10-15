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
import System.Process
import Test.Tasty.ExpectedFailure


main =
  defaultMain =<< mkTests <$> findTests

testDir = "tests/examples"

expectedFailures :: [FilePath]
expectedFailures = map (testDir </>) ["RedundantDo.hs"]

findTests :: IO [FilePath]
findTests = findByExtension [".hs"] testDir


mkTests :: [FilePath] -> TestTree
mkTests files = testGroup "Unit tests" (map mkTest files)
  where
    mkTest :: FilePath -> TestTree
    mkTest fp =
      let outfile = fp <.> "out"
          rfile   = fp <.> "refact"
          topts = Options
                  { optionsTarget       = Just fp
                  , optionsInplace       = False
                  , optionsOutput        = Just outfile
                  , optionsRefactFile    = Just rfile
                  , optionsVerbosity     = Silent
                  , optionsStep          = False
                  , optionsRoundtrip     = False
                  , optionsDebug         = False
                  , optionsVersion       = False
                  , optionsPos           = Nothing
                  }
          action = do
            (_, str, _) <- readCreateProcessWithExitCode (shell $ "hlint --serialise " ++ fp) ""
            writeFile rfile str
            hSilence [stderr] $ runPipe topts fp
          diffCmd = \ref new -> ["diff", "-u", ref, new]
          testFn  = if fp `elem` expectedFailures then expectFail else id
      in testFn $ goldenVsFileDiff fp diffCmd (fp <.> "expected") outfile action



