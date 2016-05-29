{-

Module      : CrossCourse.WebSocket.Frame
Description : Hybi13 WebSocket frame implementation
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

Implements a data structure that represents a WebSockets frame.
(as described in https://tools.ietf.org/html/rfc6455#section-5.2)

-}

{-# LANGUAGE OverloadedStrings,BangPatterns #-}

module CrossCourse.WebSocket.Frame
(
  Frame(..),
  FrameType(..),
  frameUnmaskedPayload,
  unmask
)
where

import Data.Bits
import Data.Int
import Data.Word

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- |A WebSockets frame.
data Frame = Frame {
  frameFin     :: !Bool,
  frameRsv1    :: !Bool,
  frameRsv2    :: !Bool,
  frameRsv3    :: !Bool,
  frameType    :: !FrameType,
  frameMask    :: !(Maybe B.ByteString),
  framePayload :: !BL.ByteString
} deriving (Eq,Show)

unmask :: Frame -> Frame
unmask f = f { frameMask = Nothing, framePayload = frameUnmaskedPayload f}

frameUnmaskedPayload :: Frame -> BL.ByteString
frameUnmaskedPayload (Frame _ _ _ _ _ mask pl) = maybe pl (flip maskPayload pl) mask
  where
    maskPayload mask = snd . BL.mapAccumL f 0
      where f !i !c = ((i + 1) `mod` (B.length mask),
                       (B.index mask i) `xor` c)

-- |The type of WebSockets frame.
data FrameType
    = ContinuationFrame
    | TextFrame
    | BinaryFrame
    | CloseFrame
    | PingFrame
    | PongFrame
    deriving (Eq,Show)
    
instance Binary Frame where
  get = do
    b0 <- getWord8
    b1 <- getWord8
  
    ft <- convertOpcode $ opcode b0
    
    let isMasked = b1 .&. 0x80 == 0x80
        lenflag = fromIntegral $ b1 .&. 0x7f

    len <- case lenflag of
      126 -> fromIntegral <$> getWord16be
      127 -> fromIntegral <$> getWord64be
      _   -> return lenflag

    maybeMask <- case isMasked of
      True -> Just <$> getByteString 4
      False -> return Nothing
      
    payload <- getLazyByteString len
    return $ Frame (fin b0) (rsv1 b0) (rsv2 b0) (rsv3 b0) ft maybeMask payload
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
  put f = do
    putWord8 byte0
    putWord8 byte1
    putLenOverflow
    putMaskBytes
    putLazyByteString $ framePayload f
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
      byte1 = maskflag .|. lenflag
      plen  = BL.length $ framePayload f
      (maskflag, putMaskBytes) = case frameMask f of
        Nothing -> (0x00, return ())
        Just m  -> (0x80, putByteString m)
      (lenflag, putLenOverflow)
          | plen < 126     = (fromIntegral plen, return ())
          | plen < 0x10000 = (126, putWord16be $ fromIntegral plen)
          | otherwise      = (127, putWord64be $ fromIntegral plen)