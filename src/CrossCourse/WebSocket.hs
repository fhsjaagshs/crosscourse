{-

Module      : CrossCourse.WebSocket
Description : Hybi13 WebSocket implementation
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

-}

{-# LANGUAGE BangPatterns #-}

module CrossCourse.WebSocket
(
  Frame(..),
  FrameType(..),
  parseFrame,
  encodeFrame
)
where
  
-- as described in RFC 6455
-- use
-- https://github.com/jaspervdj/websockets/blob/master/src/Network/WebSockets/Hybi13.hs
-- http://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side
-- https://tools.ietf.org/html/rfc6455#section-5

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as A

import qualified Blaze.ByteString.Builder as Bz

import Data.Bool
import Data.Bits
import Data.Int

import Data.Binary.Get (getWord16be,getWord64be,runGet)
import Data.Binary.Put (putWord16be,runPut)

-- |A WebSocket packet
data Frame = Frame {
  frameFin :: !Bool,
  frameRsv1 :: !Bool,
  frameRsv2 :: !Bool,
  frameRsv3 :: !Bool,
  frameType :: !FrameType,
  framePayload :: !BL.ByteString
} deriving (Eq, Show)

-- | The type of a frame. Not all types are allowed for all protocols.
data FrameType
    = ContinuationFrame
    | TextFrame
    | BinaryFrame
    | CloseFrame
    | PingFrame
    | PongFrame
    deriving (Eq, Show)
    
parseFrame :: B.ByteString -> Either String Frame
parseFrame = A.eitherResult . A.parse frame  
  
frame :: A.Parser Frame
frame = header <*> payload

-- |Attoparsec parser to parse first byte of WebSocket packet.
header :: A.Parser (BL.ByteString -> Frame)
header = do
  b <- A.anyWord8
  Frame (fin b) (rsv1 b) (rsv2 b) (rsv3 b) <$> (convertOpcode $ opcode b)
  where
    convertOpcode 0x00 = return ContinuationFrame
    convertOpcode 0x01 = return TextFrame
    convertOpcode 0x02 = return BinaryFrame
    convertOpcode 0x08 = return CloseFrame
    convertOpcode 0x09 = return PingFrame
    convertOpcode 0x0a = return PongFrame
    convertOpcode opcode = fail $ "unknown opcode: " ++ show opcode
    fin    b = b .&. 0x80 == 0x80
    rsv1   b = b .&. 0x40 == 0x40
    rsv2   b = b .&. 0x20 == 0x20
    rsv3   b = b .&. 0x10 == 0x10
    opcode b = b .&. 0x0f
    
-- |Attoparsec parser to parse the payload of a WebSocket packet.
payload :: A.Parser BL.ByteString
payload = do
    byte1 <- A.anyWord8
    let mask = byte1 .&. 0x80 == 0x80
        lenflag = fromIntegral (byte1 .&. 0x7f)

    len <- case lenflag of
        126 -> fromIntegral . runGet' getWord16be <$> A.take 2
        127 -> fromIntegral . runGet' getWord64be <$> A.take 8
        _   -> return lenflag

    masker <- bool (pure id) (maskPayload <$> A.take 4) mask
    masker . BL.fromChunks <$> take64 len
  where
    runGet' g = runGet g . BL.fromChunks . return

    take64 :: Int64 -> A.Parser [B.ByteString]
    take64 n
      | n <= 0    = return []
      | otherwise = do
          let n' = min intMax n
          chunk <- A.take (fromIntegral n')
          (chunk :) <$> take64 (n - n')
      where
        intMax :: Int64
        intMax = fromIntegral (maxBound :: Int)

    maskPayload :: B.ByteString -> BL.ByteString -> BL.ByteString
    maskPayload mask = snd . BL.mapAccumL f 0
      where
        len     = B.length mask
        f !i !c = (i', m `xor` c)
          where i' = (i + 1) `mod` len
                m  = mask `B.index` i
                
encodeFrame :: Frame -> Bz.Builder
encodeFrame f = mconcat [
  Bz.fromWord8 byte0,
  Bz.fromWord8 byte1,
  len,
  Bz.fromLazyByteString $ framePayload f]
  where
    byte0  = fin .|. rsv1 .|. rsv2 .|. rsv3 .|. opcode
    fin    = if frameFin  f then 0x80 else 0x00
    rsv1   = if frameRsv1 f then 0x40 else 0x00
    rsv2   = if frameRsv2 f then 0x20 else 0x00
    rsv3   = if frameRsv3 f then 0x10 else 0x00
    opcode = case frameType f of
        ContinuationFrame -> 0x00
        TextFrame         -> 0x01
        BinaryFrame       -> 0x02
        CloseFrame        -> 0x08
        PingFrame         -> 0x09
        PongFrame         -> 0x0a
    byte1 = 0x00 .|. lenflag
    len'  = BL.length $ framePayload f
    (lenflag, len)
        | len' < 126     = (fromIntegral len', mempty)
        | len' < 0x10000 = (126, Bz.fromWord16be (fromIntegral len'))
        | otherwise      = (127, Bz.fromWord64be (fromIntegral len'))
  