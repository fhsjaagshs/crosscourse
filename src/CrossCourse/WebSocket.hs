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
import Control.Monad
import Control.Monad.Fix
import System.IO

import Data.Monoid
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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
websocket hdl = fromHandle' hdl >-> parseFrames hdl >-> demuxFrames hdl >-> evalFrames hdl

-- |Creates a producer from a 'Handle'.
fromHandle' :: Handle -> Producer B.ByteString IO ()
fromHandle' h = fix $ \fx -> do
  eof <- lift $ hIsEOF h
  unless eof $ ((liftIO $ B.hGetNonBlocking h 4092) >>= yield) >> fx

-- |Parses a stream of bytes as @Frame@s.       
parseFrames :: Handle -> Pipe B.ByteString Frame IO ()
parseFrames hdl = f ""
  where l = lift . closeWebsocketCode hdl 1
        r (xs,frame) = yield frame >> f xs
        f xs = runGetWith get xs await >>= either l r

-- TODO: ignore unmasked frames

-- |Demultiplexes 'Frame's.
demuxFrames :: Handle -> Pipe Frame Frame IO ()
demuxFrames hdl = await >>= awaitConts
  where
    awaitConts f
      | frameFin f = yield f >> (await >>= awaitConts)
      | otherwise = do
        f2@(Frame fin _ _ _ typ2 _ _) <- await
        when (typ2 /= ContinuationFrame) $ lift $ closeWebsocketCode hdl 2 "unexpected continuation frame"
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
evalFrames :: Handle -> Pipe Frame Message IO ()
evalFrames hdl = fix $ \fx -> do
  f <- await
  case frameType f of
    TextFrame   -> (yield $ Message (frameUnmaskedPayload f) False) >> fx
    BinaryFrame -> (yield $ Message (frameUnmaskedPayload f) True) >> fx
    CloseFrame  -> lift $ closeWebsocket hdl (frameUnmaskedPayload f)
    PingFrame   -> do
      lift $ B.hPut hdl $ encode' $ unmask f { frameType = PongFrame }
      fx
    _   -> return ()

-- |Closes a websocket given a handle.
closeWebsocket :: Handle -> BL.ByteString -> IO ()
closeWebsocket hdl reason = do
  eof <- hIsEOF hdl
  unless eof $ (B.hPut hdl $ encode' closeFrame) -- >> hClose hdl
  where closeFrame = Frame True False False False CloseFrame Nothing reason
  
closeWebsocketCode :: Handle -> Word16 -> String -> IO ()
closeWebsocketCode hdl code reason = closeWebsocket hdl $ (runPut $ putWord16be code) <> utf8Reason
  where utf8Reason = TL.encodeUtf8 $ TL.pack reason