{-# LANGUAGE RecordWildCards #-}
module Test where

import Test.HUnit
import System.Directory
import System.FilePath

import Options.Applicative

import Main hiding (main)

import System.IO.Silently
import System.IO

import Debug.Trace

data TestOptions = TestOptions
             { optGenerate :: Bool  -- ^ Whether to regenerate test files
             }

testOptions :: Parser TestOptions
testOptions =
  TestOptions <$>
    switch (long "generate"
           <> help "Overwrite expected test results")

testOptionsWithHelp :: ParserInfo TestOptions
testOptionsWithHelp
  = info (helper <*> testOptions)
          ( fullDesc )


main = do
  o@TestOptions{..} <- execParser testOptionsWithHelp
  setCurrentDirectory "tests/examples"
  runTestTT =<< mkTests optGenerate <$> findTests

findTests :: IO [FilePath]
findTests = do
  files <- getDirectoryContents "./"
  return (filter ((== ".hs") . takeExtension ) files)


mkTests :: Bool -> [FilePath] -> Test
mkTests gen files = TestList (map mkTest files)
  where
    mkTest :: FilePath -> Test
    mkTest fp = TestCase $ do
      let outfile = fp <.> if gen then "expected" else "out"
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
      hSilence [stderr] $ runPipe topts fp
      res <- readFile outfile
      exp <- readFile (fp <.> "expected")
      exp @=? res



