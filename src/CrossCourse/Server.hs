{-# LANGUAGE OverloadedStrings #-}
module CrossCourse.Server
(
  startServer
)
where
  
import CrossCourse.WebSocket
import CrossCourse.WebSocket.HTTP

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Data.Streaming.Network (bindPortTCP)

import Data.UUID
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- import qualified Data.HashTable.IO as H

import Data.Maybe

import Pipes
import Pipes.Prelude

import Control.Monad
import Control.Exception (bracket)
import Control.Concurrent hiding (yield)

{-
TODO:
- signal/exit handlers
- track connections
  - wrap 'Socket's in 'MVar's inside any data structure
-}

type Logic = UUID -> Pipe Message (Socket,Message) IO ()

-- |Start the 'Message' server.
startServer :: Int  -- ^ port to run server on
            -> Logic -- ^ server logic based on a pipe
            -> IO ()
startServer port logic = withSocketsDo $ bindPortTCP port "*" >>= while (pure True) . acceptSocket logic

acceptSocket :: Logic -> Socket -> IO ()
acceptSocket logic lsock = bracket (accept lsock) (close' . fst) (uncurry f)
  where
    f :: Socket -> SockAddr -> IO ()
    f sock saddr = when (isSupportedSockAddr saddr) $ void $ forkIO $ do
      hs <- readHandshake r
      case hs of
        Nothing -> w $ mkBadRequestResponse "invalid handshake"
        Just (Handshake _ key hdrs) -> do
          let user = lookup "CrossCourse-User-UUID" hdrs >>= fmap (runGet get . BL.fromStrict) . listToMaybe
          case user of
            Nothing -> w $ mkBadRequestResponse "missing/invalid authentication credentials"
            Just user' -> do
              w $ mkHandshakeResponse key
              while (isConnected sock) $ runEffect $
                repeatM r >-> readEvents >-> eventLogic >-> logic user' >-> messageLogic
      where
        r = recv sock 4092
        w = sendAll sock
        eventLogic = await >>= f
          where f (ReceivedMessageEvent m) = yield m >> eventLogic
                f (CloseEvent f) = lift $ f (sendAll sock) >> close sock
                f (PingEvent f) = (lift $ f (sendAll sock)) >> eventLogic
        messageLogic = do
          (targetSock,msg) <- await
          lift $ sendAll targetSock $ encodeMessage msg

while :: IO Bool -> IO () -> IO ()          
while test act = do
  v <- test
  when v $ act >> while test act
  
close' :: Socket -> IO ()
close' s@(MkSocket _ _ _ _ status) = do
  status' <- readMVar status
  when (status' /= Closed) $ close s