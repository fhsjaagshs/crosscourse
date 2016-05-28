{-# LANGUAGE OverloadedStrings #-}
module CrossCourse.Server
(
  startServer
)
where
  
import CrossCourse.WebSocket
import CrossCourse.WebSocket.HTTP
import CrossCourse.Logic

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Data.Streaming.Network (bindPortTCP)

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
-- import Control.Monad.Fix (fix)
import Control.Concurrent hiding (yield)

{-
TODO:
- signal/exit handlers
- supply socket to logic
-}

-- |Start the 'Message' server.
startServer :: Int  -- ^ port to run server on
            -> Logic -- ^ server logic based on a pipe
            -> IO ()
startServer port logic = withSocketsDo $ do
  lsock <- bindPortTCP port "*"
  while (pure True) $ do
    tid <- acceptSocket logic port

acceptSocket :: Logic -> Socket -> IO ThreadId
acceptSocket logic lsock = accept lsock >>= uncurry f
  where
    rlen = 4092
    shakeHand sock success = do
      hs <- readHandshake $ recv sock rlen
      maybe (sendAll sock $ mkBadRequestResponse "invalid handshake") success $ do
        Handshake _ key hdrs <- hs
        user <- lookup "CrossCourse-User-UUID" hdrs >>= listToMaybe
        return $ runGet get $ BL.fromStrict user
    f sock saddr = when (isSupportedSockAddr saddr)
                 $ forkIO
                 $ shakeHand sock $ \pathInfo -> do
      auth <- newMVar Nothing
      while (isConnected sock) $ runEffect $ mkPipe auth
      -- TODO: handle disconnection
      where
        mkPipe auth = repeatM (recv sock rlen) >-> readEvents >-> eventLogic >-> logic auth >-> messageLogic
        eventLogic = await >>= f
          where f (MessageEvent m) = yield (sock,m) >> eventLogic
                f (CloseEvent f) = lift $ f (sendAll sock) >> close sock
                f (PingEvent f) = (lift $ f (sendAll sock)) >> eventLogic
        messageLogic = do
          (targetSock,msg) <- await
          lift $ sendAll targetSock $ encodeMessage msg

while :: IO Bool -> IO () -> IO ()          
while test act = do
  v <- test
  when v $ act >> while test act
  
-- close' :: Socket -> IO ()
-- close' s@(MkSocket _ _ _ _ status) = do
--   status' <- readMVar status
--   when (status' /= Closed) $ close s