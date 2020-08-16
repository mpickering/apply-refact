module Main where

import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import System.IO.Silently
import System.Process

import Refact.Internal (Verbosity(..))
import Refact.Options (Options(..))
import Refact.Run (runPipe)

import Debug.Trace
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden

main =
  defaultMain . mkTests =<< findTests

testDir = "tests/examples"

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
                  , optionsLanguage      = ["LambdaCase"]
                  , optionsPos           = Nothing
                  }
          action =
            hSilence [stderr] $ runPipe topts fp
          diffCmd = \ref new -> ["diff", "-u", ref, new]
      in goldenVsFileDiff fp diffCmd (fp <.> "expected") outfile action
