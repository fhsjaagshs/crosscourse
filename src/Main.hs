{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import CrossCourse.Server
import CrossCourse.Logic

import System.Environment
  
main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  logic <- mkLogic
  startServer port logic