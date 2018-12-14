{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Data.Maybe
import Data.Aeson
import Turtle
import Prelude hiding (FilePath)

import Skater
import Skater.Dev as Dev
import Skater.IO
import Skater.SkaterContext



parser :: Parser (FilePath, FilePath)
parser = (,) <$> argPath "SkaterContext"  "The JSON file for the SkaterContext"
             <*> argPath "Benchmark" "The JSON file containing the benchmark"

main :: IO ()
main = do
  (ctxFile, bmFile) <- options "The Skater Benchmark Suite" parser
  ut1 <- readBenchmark bmFile
  ctx <- readSkaterContext ctxFile
  run ctx ut1