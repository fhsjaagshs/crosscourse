{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}

module CrossCourse.Server
(
  startServer
)
where
  
import CrossCourse.WebSocket
import CrossCourse.WebSocket.HTTP
import CrossCourse.Logic

import Pipes
import Control.Monad
import Control.Concurrent hiding (yield)
import Control.Exception

import System.IO
import Network.Socket

{-
TODO:
- signal/exit handlers: close all websockets
  - de-auth: closing the connection should be as simple as exiting the pipe loop
-}

-- |Start a given websocket server given a port and 'Logic'.
startServer :: Int  -- ^ port to run server on
            -> Logic -- ^ server logic based on a pipe
            -> IO ()
startServer port logic = withSocketsDo $ tcpSocket port >>= f
  where f Nothing = error "failed to bind TCP socket."
        f (Just lsock) = forever $ do
          (sock,saddr) <- accept lsock
          if isSupportedSockAddr saddr
            then do
              hdl <- socketToHandle sock ReadWriteMode
              void $ forkIO $ wsHandshake hdl $ do
                auth <- newMVar Nothing
                -- TODO: fix this pipe line terminating
                runEffect $ websocket hdl >-> logic auth hdl
                putStrLn "after"
                closeWebsocket hdl ""
            else close sock
            
-- |Bind to a TCP socket given a port using 'getAddrInfo' to
-- find the appropriate binding addr. Adapted from 'bindPortTCP'
-- in streaming-commons.
tcpSocket :: Int -> IO (Maybe Socket)
tcpSocket port = tryAddrs =<< getAddr
  where
    hints = defaultHints {
              addrFlags = [AI_PASSIVE,AI_NUMERICSERV,AI_NUMERICHOST],
              addrSocketType = Stream
            }
    getAddr = getAddrInfo (Just hints) Nothing (Just $ show port)
    tryAddrs []     = return Nothing
    tryAddrs (x:xs) = catch (Just <$> f x) (\(_ :: IOException) -> tryAddrs xs)
    f a = bracketOnError open close inBetween
      where
        open = socket (addrFamily a) (addrSocketType a) (addrProtocol a)
        inBetween sock = do
          setSocketOption sock NoDelay 1
          setSocketOption sock ReuseAddr 1
          bindSocket sock (addrAddress a)
          listen sock (max 2048 maxListenQueue)
          return sock