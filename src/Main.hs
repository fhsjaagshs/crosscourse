{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import CrossCourse.HTTP
import CrossCourse.Protocol
  
import System.Environment
  
main :: IO ()
main = lookupEnv "PORT" >>= startWebSocket handler . maybe 8080 read

handler :: Message -> (Message -> IO ()) -> IO ()
handler message sendMessage = sendMessage message 