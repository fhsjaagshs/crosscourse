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
  closeWebsocket
)
where

{-
TODO
- chunk larger text/binary frames, see 'encodeMessage'
-}

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
  
data Message = Message {
  messagePayload :: !BL.ByteString,
  messageIsBinary :: !Bool
} deriving (Eq,Show)
  
-- |WebSocket logic. Responds to incoming frames, closing @hdl@ if necessary.
-- Yields 'Message's to be used in a pipeline.
websocket :: Handle -> Producer Message IO ()
websocket hdl = fromHandle' hdl >-> parseFrames >-> demuxFrames >-> evalFrames hdl
  where
    fromHandle' h = fix $ \next -> do
      eof <- lift $ hIsEOF h
      unless eof $ do
        (liftIO $ B.hGet h 4092) >>= yield
        next

-- |Parses a stream of bytes as @Frame@s.
parseFrames :: Pipe B.ByteString Frame IO ()
parseFrames = loop ""
  where
    parse :: B.ByteString -> [Frame] -> Either String (B.ByteString,[Frame])
    parse bs accum = d $ runGetIncremental get
      where d (Partial f) = d $ f $ Just bs
            d (Done remaining _ result) = parse remaining (accum ++ [result])
            d (Fail "" _ _) = Right (bs,accum) -- consumed @bs@, not enough input to finish
            d (Fail _ _ err) = Left err -- encountered an error parsing
    loop = fix $ \next bs -> await >>= either l (r next) . flip parse [] . mappend bs
      where
        r next ("",frames) = mapM_ yield frames
        r next (leftover,frames) = mapM_ yield frames >> next leftover
        l = yield . Frame True False False False CloseFrame Nothing . mappend (runPut $ putWord16be 1) . BLC.pack

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
      lift $ B.hPut hdl $ encodeFrame $ unmask f { frameType = PongFrame }
      next
    PongFrame   -> return () -- ignore pong frames

-- TODO: close a websocket given a failure reason and a message.
-- |Closes a websocket given a handle.
closeWebsocket :: Handle -> BL.ByteString -> IO ()
closeWebsocket hdl reason = do
  eof <- hIsEOF hdl
  unless eof $ (B.hPut hdl $ encodeFrame closeFrame) >> hClose hdl
  where closeFrame = Frame True False False False CloseFrame Nothing reason
  
encodeFrame :: Frame -> B.ByteString
encodeFrame = BL.toStrict . runPut . put

-- TODO: chunking via continuation frames
encodeMessage :: Message -> B.ByteString
encodeMessage (Message p True) = encodeFrame $ Frame True False False False BinaryFrame Nothing p
encodeMessage (Message p False) = encodeFrame $ Frame True False False False TextFrame Nothing p