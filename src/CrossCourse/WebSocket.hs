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

{-# LANGUAGE TupleSections,FlexibleContexts,OverloadedStrings #-}

module CrossCourse.WebSocket
(
  Message(..),
  module CrossCourse.WebSocket.Monad,
  websocket,
  messageSink
)
where

import CrossCourse.WebSocket.Frame
import CrossCourse.WebSocket.Monad

import Pipes
import System.IO

import Data.Monoid
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- |Represents a message from a WebSocket.
data Message = Message {
  messagePayload :: !BL.ByteString, -- ^ the message's payload (from its originating 'Frame')
  messageIsBinary :: !Bool -- ^ whether the message is binary
} deriving (Eq,Show)

-- |WebSocket logic. Responds to incoming frames, closing @hdl@ if necessary.
-- Yields 'Message's to be used in a pipeline.
websocket :: MonadIO m => Producer Message (WebSocketT m) ()
websocket = frames >-> ensureSane >-> demuxFrames >-> evalFrames 

frames :: MonadIO m => Producer Frame (WebSocketT m) ()
frames = do
  h <- lift handle
  let f xs = runGetWith get (src h) xs >>= either l r
      l = lift . invalidData
      r (xs',frame) = yield frame >> f xs'
  f "" -- will run forever
  where
    src h = liftIO $ do
      unavailable <- hIsClosed h
      if unavailable
        then return Nothing
        else Just <$> B.hGetNonBlocking h 4092
        
runGetWith :: (Monad m, Binary a)
           => Get a
           -> m (Maybe B.ByteString) -- ^ action to read in data
           -> B.ByteString -- ^ data to feed in that was leftover from previous runs
           -> m (Either String (B.ByteString,a))
runGetWith g src = d . pushChunk (runGetIncremental g)
  where d (Partial f) = src >>= d . f
        d (Done leftover _ result) = return $ Right (leftover,result)
        d (Fail _ _ err) = return $ Left err

-- |Ensures frames are masked & secure, as per the WebSockets RFC.
ensureSane :: MonadIO m => Pipe Frame Frame (WebSocketT m) ()
ensureSane = forever $ await >>= go
  where go f
          | not $ isMasked f = lift $ protocolError "unexpected unmasked message"
          | isControl f && (not $ frameFin f) = lift $ protocolError "control frames cannot be fragmented"
          -- TODO: ensure text frames are UTF-8
          | otherwise = yield f

-- |Demultiplexes 'Frame's.
demuxFrames :: MonadIO m => Pipe Frame Frame (WebSocketT m) ()
demuxFrames = forever $ awaitFrame isData >>= awaitConts
  where
    awaitFrame p = do
      f <- await
      if isControl f then yield f >> awaitFrame p
        else if p f then return f
          else lift $ protocolError $ "unexpected " ++ (show $ frameType f)
    awaitConts f
      | frameFin f = yield f
      | otherwise = do
        f2 <- awaitFrame isContinuation
        awaitConts $ f {
          frameFin = frameFin f2,
          frameMask = Nothing,
          framePayload = frameUnmaskedPayload f <> frameUnmaskedPayload f2
        }
        
-- |Responds to incoming frames:
-- * 'yield's 'Message's on 'TextFrame's and 'BinaryFrame's
-- * Closes the connection on 'CloseFrame's
-- * Sends 'PongFrame's on 'PingFrame's
-- * Ignores all other frames
evalFrames :: MonadIO m => Pipe Frame Message (WebSocketT m) ()
evalFrames = forever $ await >>= go
  where
    go f@(Frame _ _ _ _ TextFrame _ _) = yield $ Message (frameUnmaskedPayload f) False
    go f@(Frame _ _ _ _ BinaryFrame _ _) = yield $ Message (frameUnmaskedPayload f) True
    go f@(Frame _ _ _ _ CloseFrame _ _) = throwError $ frameUnmaskedPayload f
    go f@(Frame _ _ _ _ PingFrame _ _) = lift $ do
      h <- handle
      liftIO $ BL.hPut h $ encode $ unmask f { frameType = PongFrame }
    go   (Frame _ _ _ _ PongFrame _ _) = return ()
    go   (Frame _ _ _ _ ContinuationFrame _ _) = lift $ protocolError "unexpected continuation frame"
    
-- |Consume messaging targeting tuples
messageSink :: MonadIO m => Consumer (Maybe (Handle,Message)) (WebSocketT m) ()
messageSink = forever $ await >>= maybe (pure ()) f
  where f (dest,msg) = liftIO $ BL.hPut dest $ encode $ mkFrame msg-- hPutBinary dest $ mkFrame msg
        mkFrame (Message p False) = Frame True False False False TextFrame Nothing p
        mkFrame (Message p True) = Frame True False False False BinaryFrame Nothing p