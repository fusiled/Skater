{-# LANGUAGE OverloadedStrings #-}
module Skater.Log where

import Skater.SkaterContext

import Turtle
import Control.Monad


data LogSeverity = Info | Warn | Fine | Error | Time deriving (Show)

logMsg :: SkaterContext -> LogSeverity -> Text -> IO()
logMsg context severity msg = do
                              ioDate <- date
                              let messageLine = unsafeTextToLine  ( mconcat ["[", repr severity,"]:[",repr ioDate,"]: ",msg]);
                              append (logPath context) (return messageLine)
                              when (forwardLogToStdErr context) $ Turtle.stderr (return messageLine)

logWarn :: SkaterContext -> Text -> IO()
logWarn context msg  = logMsg context Warn  msg

logInfo :: SkaterContext -> Text -> IO()
logInfo context msg  = logMsg context Info msg

logFine ::  SkaterContext -> Text -> IO()
logFine  context msg = logMsg context Fine msg

logError :: SkaterContext -> Text -> IO()
logError context msg = logMsg context Error msg

logTime :: SkaterContext -> Text -> IO()
logTime context msg = logMsg context Time msg