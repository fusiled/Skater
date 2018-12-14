{-# LANGUAGE OverloadedStrings #-}
module Skater.JsonInternal where

import Turtle
import Data.Aeson


instance FromJSON Turtle.FilePath where
  parseJSON el = case el of String x -> (return . fromText) x