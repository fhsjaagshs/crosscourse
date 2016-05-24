{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import CrossCourse.HTTP
import CrossCourse.Protocol
  
import System.Environment
  
main :: IO ()
main = lookupEnv "PORT" >>= startWebSocket . maybe 8080 read