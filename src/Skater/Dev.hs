{-# LANGUAGE OverloadedStrings #-}
module Skater.Dev where

import Turtle

-- Devices
stdout :: Turtle.FilePath
stdout = "/dev/stdout"

stderr :: Turtle.FilePath
stderr = "/dev/stderr"


stdin :: Turtle.FilePath
stdin = "/dev/stdin"

null :: Turtle.FilePath
null = "/dev/null"

zero :: Turtle.FilePath
zero = "/dev/zero"
