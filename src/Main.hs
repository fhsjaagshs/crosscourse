{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import CrossCourse.Server

import Pipes
  
import System.Environment

-- TODO: crosscourse protocol
  
main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  startServer port app
  
app :: Pipe Message Message IO ()
app = do
  msg <- await
  yield $ Message "That's what she said!" False