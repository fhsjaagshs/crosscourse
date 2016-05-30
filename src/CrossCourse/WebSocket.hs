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

{-# LANGUAGE OverloadedStrings #-}

module CrossCourse.WebSocket
(
  Message(..),
  websocket,
  closeWebsocket,
  closeWebsocketCode
)
where

import CrossCourse.Binary
import CrossCourse.WebSocket.Frame

import Pipes
import qualified Pipes.Prelude as P
import Control.Monad
import Control.Monad.Fix
import System.IO

import Data.Monoid
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
  
data Message = Message {
  messagePayload :: !BL.ByteString,
  messageIsBinary :: !Bool
} deriving (Eq,Show)

instance Binary Message where
  put (Message p False) = put $ Frame True False False False TextFrame Nothing p
  put (Message p True) = put $ Frame True False False False BinaryFrame Nothing p
  get = fail "cannot read a message from binary"
  
-- |WebSocket logic. Responds to incoming frames, closing @hdl@ if necessary.
-- Yields 'Message's to be used in a pipeline.
websocket :: Handle -> Producer Message IO ()
websocket hdl = fromHandle' hdl >-> parseFrames >-> demuxFrames >-> evalFrames hdl

-- |Creates a producer from a 'Handle'.
fromHandle' :: Handle -> Producer B.ByteString IO ()
fromHandle' h = fix $ \next -> do
  eof <- lift $ hIsEOF h
  unless eof $ do
    (liftIO $ B.hGetNonBlocking h 4092) >>= yield
    next
   
-- |Parses a stream of bytes as @Frame@s.       
parseFrames :: Pipe B.ByteString Frame IO ()
parseFrames = loop ""
  where
    mkClose = Frame True False False False CloseFrame Nothing
    l = yield . mkClose . mappend (runPut $ putWord16be 1) . BLC.pack
    r next (remaining,frame) = yield frame >> next remaining
    loop = fix $ \next leftover -> runGetWith get leftover await >>= either l (r next)

-- |Demultiplexes 'Frame's.
demuxFrames :: Pipe Frame Frame IO ()
demuxFrames = fix $ \next -> (await >>= awaitConts) >> next
  where
    awaitConts f
      | frameFin f = yield f
      | otherwise = do
        f2@(Frame fin _ _ _ typ2 _ _) <- await
        when (typ2 /= ContinuationFrame) $ fail "expected continuation frame"
        awaitConts $ f {
          frameFin = fin,
          frameMask = Nothing,
          framePayload = frameUnmaskedPayload f <> frameUnmaskedPayload f2
        }
        
-- |Responds to frames, closing @hdl@ on a 'CloseFrame', sending a
-- 'PongFrame' on a 'PingFrame', ignoring 'PongFrame's, and
-- yielding messages on 'TextFrame's and 'BinaryFrame's.
evalFrames :: Handle -> Pipe Frame Message IO ()
evalFrames hdl = fix $ \next -> do
  f <- await
  case frameType f of
    TextFrame   -> (yield $ Message (frameUnmaskedPayload f) False) >> next
    BinaryFrame -> (yield $ Message (frameUnmaskedPayload f) True) >> next
    CloseFrame  -> lift $ closeWebsocket hdl (frameUnmaskedPayload f)
    PingFrame   -> do
      lift $ B.hPut hdl $ encode' $ unmask f { frameType = PongFrame }
      next
    PongFrame   -> return () -- ignore pong frames

-- |Closes a websocket given a handle.
closeWebsocket :: Handle -> BL.ByteString -> IO ()
closeWebsocket hdl reason = do
  eof <- hIsEOF hdl
  unless eof $ (B.hPut hdl $ encode' closeFrame) >> hClose hdl
  where closeFrame = Frame True False False False CloseFrame Nothing reason
  
closeWebsocketCode :: Handle -> Word16 -> String -> IO ()
closeWebsocketCode hdl code reason = closeWebsocket hdl $ encode code <> utf8Reason
  where utf8Reason = TL.encodeUtf8 $ TL.pack reason