{-

Module      : CrossCourse.WebSocket
Description : Hybi13 WebSocket implementation
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

Implements a 'Pipe' that supports the WebSockets protocol
(as described in https://tools.ietf.org/html/rfc6455)

-}

{-# LANGUAGE OverloadedStrings,GeneralizedNewtypeDeriving #-}

module CrossCourse.WebSocket
(
  Message(..),
  WebSocketT,
  runWebSocketT,
  handle,
  websocket,
  messageSink,
  closeWS,
  closeWSCode
)
where

import CrossCourse.Binary
import CrossCourse.WebSocket.Frame

import Pipes
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import System.IO

import Data.Monoid
import Data.Maybe
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- |Represents a message from a WebSocket.
data Message = Message {
  messagePayload :: !BL.ByteString, -- ^ the message's payload (from its originating 'Frame')
  messageIsBinary :: !Bool -- ^ whether the message is binary
} deriving (Eq,Show)

-- |Monad around WebSocket.
newtype WebSocketT m a = WebSocketT (ReaderT Handle m a)
  deriving (Functor,Applicative,Monad,MonadTrans,MonadIO)
  
runWebSocketT :: WebSocketT m a -> Handle -> m a
runWebSocketT (WebSocketT r) = runReaderT r

-- |The the wrapped 'Handle' out of a 'WebSocketT'.
handle :: Monad m => WebSocketT m Handle
handle = WebSocketT ask
  
-- |WebSocket logic. Responds to incoming frames, closing @hdl@ if necessary.
-- Yields 'Message's to be used in a pipeline.
websocket :: MonadIO m => Producer Message (WebSocketT m) ()
websocket = fromHandle' >-> parseFrames >-> ensureMasked >-> demuxFrames >-> evalFrames

-- |Creates a producer from a 'Handle'.
fromHandle' :: MonadIO m => Producer B.ByteString (WebSocketT m) ()
fromHandle' = do
  h <- lift handle
  fix $ \loop -> do
    eof <- liftIO $ hIsEOF h
    unless eof $ do
      bs <- liftIO $ B.hGetNonBlocking h 4092
      yield bs
      loop

-- |Parses a stream of bytes as @Frame@s.       
parseFrames :: MonadIO m => Pipe B.ByteString Frame (WebSocketT m) ()
parseFrames = f ""
  where l = lift . closeWSCode 1
        r (xs,frame) = yield frame >> f xs
        f xs = runGetWith get xs await >>= either l r

ensureMasked :: MonadIO m => Pipe Frame Frame (WebSocketT m) ()
ensureMasked = fix $ \fx -> do
  f <- await
  if isJust $ frameMask f
    then yield f >> fx
    else lift $ closeWSCode 2 "unexpected unmasked message."

-- |Demultiplexes 'Frame's.
demuxFrames :: MonadIO m => Pipe Frame Frame (WebSocketT m) ()
demuxFrames = await >>= awaitConts
  where
    awaitConts f
      | frameFin f = yield f >> (await >>= awaitConts)
      | otherwise = do
        f2@(Frame fin _ _ _ typ2 _ _) <- await
        when (typ2 /= ContinuationFrame) $ lift $ closeWSCode 2 "unexpected continuation frame"
        awaitConts $ f {
          frameFin = fin,
          frameMask = Nothing,
          framePayload = frameUnmaskedPayload f <> frameUnmaskedPayload f2
        }
        
-- |Responds to incoming frames:
-- * 'yield's 'Message's from 'TextFrame's and 'BinaryFrame's
-- * Closes the connection on 'CloseFrame's
-- * Sends 'PongFrame's on 'PingFrame's
-- * Ignores all other frames
evalFrames :: MonadIO m => Pipe Frame Message (WebSocketT m) ()
evalFrames = do
  hdl <- lift handle
  fix $ \fx -> do
    f <- await
    case frameType f of
      TextFrame   -> (yield $ Message (frameUnmaskedPayload f) False) >> fx
      BinaryFrame -> (yield $ Message (frameUnmaskedPayload f) True) >> fx
      CloseFrame  -> lift $ closeWS $ frameUnmaskedPayload f
      PingFrame   -> do
        (liftIO $ hPutBinary hdl $ unmask f { frameType = PongFrame }) >> fx
      _   -> return ()
    
-- |Consume messaging targeting tuples
messageSink :: MonadIO m => Consumer (Maybe (Handle,Message)) (WebSocketT m) ()
messageSink = fix $ \go -> (await >>= f) >> go
  where f (Just (dest,msg)) = liftIO $ hPutBinary dest $ mkFrame msg
        f _ = pure ()
        mkFrame (Message p False) = Frame True False False False TextFrame Nothing p
        mkFrame (Message p True) = Frame True False False False BinaryFrame Nothing p

-- |Closes a WebSocket given a binary payload
closeWS :: MonadIO m => BL.ByteString -> WebSocketT m ()
closeWS payload = do
  h <- handle
  liftIO $ do
    eof <- hIsEOF h
    unless eof $ (hPutBinary h $ closeFrame) >> hClose h
  where closeFrame = Frame True False False False CloseFrame Nothing payload
  
-- |Close a websocket with an error code and message.
closeWSCode :: MonadIO m => Word16 -> String -> WebSocketT m ()
closeWSCode code message = closeWS $ (runPut $ putWord16be code) <> utf8Reason
  where utf8Reason = TL.encodeUtf8 $ TL.pack message
  
-- TODO: Proper WebSocket error codes:
-- http://stackoverflow.com/questions/18803971/websocket-onerror-how-to-read-error-description
-- Danielle Ensign's answer
  