{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types

import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)
import Refact.Perform
import Refact.Utils (toGhcSrcSpan)

import System.Environment
import System.Process
import System.Directory
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pipe", file] -> runPipe file
    _ -> error "to implement"


-- Pipe

runPipe :: FilePath -> IO ()
runPipe file = do
  path <- canonicalizePath file
  rawhints <- getHints path
  let inp :: [Refactoring R.SrcSpan] = read rawhints
  print inp
  let inp' = fmap (toGhcSrcSpan file) <$> inp
  (anns, m) <- either (error . show) id <$> parseModule file
  putStrLn (showGhc anns)
  let as = relativiseApiAnns m anns
      -- need a check here to avoid overlap
  let    (ares, res) = foldl (uncurry runRefactoring) (as, m) inp'
--  putStrLn $ showGhc (Map.toAscList (fst as))
--  putStrLn $ showGhc (Map.toAscList (fst ares))
  putStrLn (exactPrintWithAnns res ares)

-- Run HLint to get the commands

makeCmd :: String -> String
makeCmd file = "hlint " ++ file ++ " --serialise"
--makeCmd _ = "runghc-7.10.1.20150609 -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.1.20150609-packages.conf.d/ feeder.hs"

getHints :: FilePath -> IO String
getHints file = do
  (_, Just hOut, _, hProc) <- createProcess (
                                (shell (makeCmd file))
                                { std_out = CreatePipe }
                              )
  () <$ waitForProcess hProc
  hGetContents hOut

