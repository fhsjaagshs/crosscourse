{-# LANGUAGE OverloadedStrings #-}
module CrossCourse.Server
(
  Message(..),
  startServer
)
where
  
import CrossCourse.WebSocket
import CrossCourse.WebSocket.HTTP

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Data.Streaming.Network (bindPortTCP)

import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Bool
import Data.Maybe
import Data.Either
import Data.Monoid

import Data.Binary
import Data.Bits

import Pipes
import Pipes.Prelude
import qualified Pipes.Prelude as P

import Data.IORef

import Control.Monad
import Control.Exception (bracket)
import Control.Concurrent (myThreadId,killThread)

{-

TODO:
- signal/exit handlers
- forking for multiple connections at once
- flush body after reading headers

-}

-- |Start the 'Message' server.
startServer :: Int  -- ^ port to run server on
            -> Pipe Message Message IO () -- ^ pipe for receiving and sending messages
            -> IO ()
startServer port pipe = withSocketsDo $ bindPortTCP port "*" >>= loop
  where loop sock = acceptSocket sock pipe >> loop sock

acceptSocket :: Socket -> Pipe Message Message IO () -> IO ()
acceptSocket lsock logic = bracket (fst <$> accept lsock) close $ \sock -> do
  lock <- newIORef False
  let r = recv sock 4092
      w = sendAll sock
      loop :: IO ()
      loop = do
        closed <- readIORef lock
        when (not closed) $ do
          runEffect $ repeatM r >-> readEvents >-> handleEvents lock w >-> logic >-> sendMessage w
          loop
  
  hs <- readHandshake r
  Prelude.print hs
  case hs of
    Nothing -> w $ mkBadRequestResponse "invalid handshake"
    Just (Handshake _ key _) -> do
      w $ mkHandshakeResponse key
      loop

handleEvents :: IORef Bool -> (B.ByteString -> IO ()) -> Pipe Event Message IO ()
handleEvents lock sink = (await >>= f) >> handleEvents lock sink
  where
    f (ReceivedMessageEvent msg) = yield msg
    f (CloseEvent f) = lift $ (f sink) >> writeIORef lock True
    f (PingEvent f) = lift $ f sink
    
sendMessage :: (B.ByteString -> IO ()) -> Consumer Message IO ()
sendMessage w = await >>= lift . w . encodeMessage