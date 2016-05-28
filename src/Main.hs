{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import CrossCourse.Server
import CrossCourse.Logic

import System.Environment
  
main :: IO ()
main = startServer
       <$> (maybe 8080 read <$> lookupEnv "PORT")
       <*> mkLogic