{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import CrossCourse.Server
import CrossCourse.WebSocket

import Pipes

import Data.UUID
  
import System.Environment


{-
TODO

Chat logic
- finish "crosscourse" protocol
  - URI carries path UUID??
- authentication
-}
  
main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  startServer port app
  
app :: UUID -> Pipe Message Message IO ()
app userid = do
  Message msg isBinary <- await
  yield $ Message (mappend msg "? That's what she said!") False