{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CrossCourse.WebSocket.Monad
(
  WebSocketT(..),
  module Control.Monad.Reader,
  module Control.Monad.Except,
  runWebSocketT,
  handle,
  closeWebsocket,
  closeNormal,
  goingAway,
  protocolError,
  invalidData,
  inconsistentData,
  violatedPolicy,
  messageTooBig,
  cannotFulfill
)
where
  
import CrossCourse.Binary
import CrossCourse.WebSocket.Frame

import Data.Word
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Binary.UTF8.Generic as UTF8
  
import Control.Monad.Reader
import Control.Monad.Except

import System.IO

import Debug.Trace

-- TODO: implement without a monad transformer stack for speed
-- TODO: fix this, it doesn't work
--
-- |Monad around WebSocket. It's a ReaderT around an ErrorT. The idea is that
-- closing the connection can be done with MonadError functions so that managing
-- sockets and erroring out can be done with more ease/confidence.
newtype WebSocketT m a = WebSocketT (ReaderT Handle (ExceptT BL.ByteString m) a)
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            MonadError BL.ByteString)

-- |Run a websocket.
runWebSocketT :: MonadIO m => WebSocketT m a -> Handle -> m (Maybe a)
runWebSocketT (WebSocketT rdr) hdl = (runExceptT $ runReaderT rdr hdl) >>= either (l . trace "doing L") (r . trace "doing r")
  where
    mkClose = Frame True False False False CloseFrame Nothing
    r = return . Just
    l err = liftIO $ do
      eof <- hIsEOF hdl
      putStrLn "closeframe" 
      unless eof $ (hPutBinary hdl $ mkClose err) >> putStrLn "closeframe" >> hClose hdl
      return Nothing

-- |The the wrapped 'Handle' out of a 'WebSocketT'.
handle :: Monad m => WebSocketT m Handle
handle = WebSocketT ask

-- |Close a WebSocket with an error code and a message. Exits
-- websocket logic immediately.
closeWebsocket :: MonadIO m => Word16 -> String -> WebSocketT m a
closeWebsocket code reason = throwError payload
  where payload = (runPut $ putWord16be code) `mappend` UTF8.fromString reason 
        
-- |Normal WebSocket closure.
closeNormal :: MonadIO m => WebSocketT m a
closeNormal = closeWebsocket 1000 "success"

-- |For example: server shutdown, browser navigating away.
goingAway :: MonadIO m => WebSocketT m a
goingAway = closeWebsocket 1001 "going away"

-- |Protocol error.
protocolError :: MonadIO m => String -> WebSocketT m a
protocolError = closeWebsocket 1002

-- |Endpoint received invalid data: eg receiving text when expecting binary.
invalidData :: MonadIO m => String -> WebSocketT m a
invalidData = closeWebsocket 1003

-- |Endpoint received data within a message that was not consistent with the
-- type of message (eg non-UTF8 data in a text frame).
inconsistentData :: MonadIO m => String -> WebSocketT m a
inconsistentData = closeWebsocket 1007

-- |General error code when a request cannot be processed.
violatedPolicy :: MonadIO m => String -> WebSocketT m a
violatedPolicy = closeWebsocket 1008

-- |Message was too big to process.
messageTooBig :: MonadIO m => WebSocketT m a
messageTooBig = closeWebsocket 1009 "message too big"

-- |Cannot fulfill a request because of a condition.
cannotFulfill :: MonadIO m => String -> WebSocketT m a
cannotFulfill = closeWebsocket 1011

-- TODO: Proper WebSocket error codes:
-- http://stackoverflow.com/questions/18803971/websocket-onerror-how-to-read-error-description
-- Danielle Ensign's answer
  