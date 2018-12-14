{-# LANGUAGE OverloadedStrings #-}
module Skater.IO where

import Skater

import Turtle 
import Prelude hiding (FilePath)
import Data.Aeson
import Data.ByteString.Lazy as BSL
import Skater.SkaterContext

checkFileExistence :: String -> IO ()
checkFileExistence path = do
  exists <- testfile $ fromString path
  if not exists
  then error $ mconcat ["File ", path, " does not exists or it's a folder"]
  else return ()


readSkaterContext :: FilePath -> IO SkaterContext
readSkaterContext fPath = do
  checkFileExistence path
  ct <- BSL.readFile path
  case decode ct :: Maybe SkaterContext of
    Just skctx -> return skctx
    Nothing -> error $ mconcat ["Cannot read and/or decode skatercode at path", path]
  where path = encodeString fPath

readBenchmark :: FilePath -> IO Benchmark
readBenchmark fPath = do
  checkFileExistence path
  tu <- BSL.readFile path
  case decode tu :: Maybe Benchmark of
    Just bench -> return bench
    Nothing -> error $ mconcat ["Cannot read and/or decode benchmark at path", path]
  where path = encodeString fPath
