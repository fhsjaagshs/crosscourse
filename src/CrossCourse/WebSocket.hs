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
  Event(..),
  Message(..),
  encodeMessage,
  readEvents,
  closeFrame,
  encodeFrame
)
where

-- refer to
-- https://tools.ietf.org/html/rfc6455#section-5

{-
TODO

- chunk larger text/binary frames, see 'encodeMessage'

-}

import CrossCourse.WebSocket.Frame

import Pipes
import Control.Monad

import Data.Monoid
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- |function that takes a "sink" function and uses it to write a frame.
type FrameWriter = (B.ByteString -> IO ()) -> IO ()
    
data Event
  = MessageEvent Message
  | CloseEvent FrameWriter
  | PingEvent FrameWriter
  
data Message = Message {
  messagePayload :: !BL.ByteString,
  messageIsBinary :: !Bool
} deriving (Eq,Show)
  
-- |Reads WebSocket 'Frame's and constructs 'Event's based on them.
readEvents :: Pipe B.ByteString Event IO ()
readEvents = parserPipe >-> demuxPipe >-> ignorePongPipe >-> eventifyPipe

-- |Parses a stream of socket data as @Frame@s.
parserPipe :: Pipe B.ByteString Frame IO ()
parserPipe = loop ""
  where
    parse :: B.ByteString -> [Frame] -> Either String (B.ByteString,[Frame])
    parse "" accum = Right ("",accum)
    parse bs accum = d $ runGetIncremental get
      where
        d (Partial f) = d $ f $ Just bs
        d (Done remaining _ result) = parse remaining (accum ++ [result])
        d (Fail remaining consumedLen err)
          | (B.null remaining) && (consumedLen == (fromIntegral $ B.length bs)) = Right (bs,accum)
          | otherwise = Left err
    loop bs = do
      s <- await
      case parse (bs <> s) [] of
        Left err -> lift $ print err
        Right (leftover,frames) -> do
          mapM_ yield frames
          when (not $ B.null leftover) $ loop leftover
  
-- |Demultiplexes 'Frame's.
demuxPipe :: Pipe Frame Frame IO ()
demuxPipe = (await >>= awaitConts) >> demuxPipe
  where
    awaitConts f@(Frame True _ _ _ _ _ _) = yield f
    awaitConts (Frame False rsv1 rsv2 rsv3 typ mask pl) = do
      (Frame fin _ _ _ typ2 mask2 pl2) <- await
      if typ2 == ContinuationFrame
        then awaitConts $ Frame fin rsv1 rsv2 rsv3 typ mask (pl <> pl2) -- TODO: properly handle masking concat'd frames
        else fail "expected continuation frame"

-- |Ignore pong 'Frame's because we're not supposed to respond to them.
ignorePongPipe :: Pipe Frame Frame IO ()
ignorePongPipe = do
  f <- await
  when ((frameType f) /= PongFrame) $ yield f
  ignorePongPipe
  
-- |Construct 'Event's based on 'Frame's.
eventifyPipe :: Pipe Frame Event IO ()
eventifyPipe = do
  (Frame _ _ _ _ typ mask pl) <- await
  let pl' = maybe pl (flip maskPayload pl) mask
  case typ of
    TextFrame -> yield $ MessageEvent $ Message pl' False
    BinaryFrame -> yield $ MessageEvent $ Message pl' True
    CloseFrame -> yield $ CloseEvent ($ encodeFrame $ construct CloseFrame pl') -- Frame True False False False CloseFrame Nothing pl)
    PingFrame -> yield $ PingEvent ($ encodeFrame $ construct PongFrame pl') -- Frame True False False False PongFrame Nothing pl)
    _ -> return ()
  eventifyPipe
  where
    construct typ payload = Frame True False False False typ Nothing payload
  
sendClose :: (B.ByteString -> IO ()) -> IO ()
sendClose = ($ encodeFrame closeFrame)
  
encodeFrame :: Frame -> B.ByteString
encodeFrame = BL.toStrict . runPut . put

closeFrame :: Frame
closeFrame = Frame True False False False CloseFrame Nothing ""

-- TODO chunking via continuation frames
encodeMessage :: Message -> B.ByteString
encodeMessage (Message p True) = encodeFrame $ Frame True False False False BinaryFrame Nothing p
encodeMessage (Message p False) = encodeFrame $ Frame True False False False TextFrame Nothing p