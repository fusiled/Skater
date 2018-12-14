{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Skater.SkaterContext where

import Turtle

import Skater.JsonInternal
import Data.Aeson
import GHC.Generics

data SkaterContext = SkaterContext { forwardLogToStdErr :: Bool
                                   , logPath :: Turtle.FilePath 
                                   } deriving (Show,Generic)

instance FromJSON SkaterContext

defaultSkaterContext = SkaterContext{ forwardLogToStdErr = True, logPath="./log.log"}