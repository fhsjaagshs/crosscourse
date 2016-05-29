{-# LANGUAGE OverloadedStrings #-}
module CrossCourse.Server
(
  startServer
)
where
  
import CrossCourse.WebSocket
import CrossCourse.WebSocket.HTTP
import CrossCourse.Logic

import Data.Streaming.Network (bindPortTCP)

import System.IO

import Data.UUID
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Maybe

import Pipes
import Pipes.Prelude

import Control.Monad
import Control.Monad.Fix (fix)
import Control.Concurrent hiding (yield)

{-
TODO:
- signal/exit handlers: close all sockets
- supply socket/handle to logic
-}

-- |Start a given websocket server given a port and 'Logic'.
startServer :: Int  -- ^ port to run server on
            -> Logic -- ^ server logic based on a pipe
            -> IO ()
startServer port logic = withSocketsDo $ do
  lsock <- bindPortTCP port "*" 
  forever $ do
    (sock,saddr) <- accept lsock
    if (isSupportedSockAddr saddr)
      then do
        hdl <- socketToHandle sock ReadWriteMode
        forkIO $ shakeHand hdl $ do
          auth <- newMVar Nothing
          runEffect $ websocket hdl >-> logic auth hdl >-> messageLogic
          closeWebsocket hdl ""
      else close sock
  where
    messageLogic = do
      (dest,msg) <- await
      lift $ hPut dest $ encodeMessage msg