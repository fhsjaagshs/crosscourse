{-

Module      : CrossCourse.WebSocket.HTTP
Description : HTTP essentials for WebSockets
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

Implements basic HTTP functionality needed for running websockets.

-}

{-# LANGUAGE OverloadedStrings #-}

module CrossCourse.WebSocket.HTTP
(
  wsHandshake,
  splitPath
)
where
  
import CrossCourse.WebSocket.RFC2616

import Control.Monad

import Data.List
import Data.Maybe
import Data.Bool
import Data.Char
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Data.Binary
import Data.Binary.Put
import Data.Attoparsec.ByteString
import Crypto.Hash (Digest,SHA1,hash)
import Data.ByteArray (convert)

import System.IO

-- |Split a path into path components
splitPath :: B.ByteString -> [B.ByteString]
splitPath = catMaybes . unfoldr splitter
  where
    splitter bs
      | B.null bs = Nothing
      | B.null t = Just (Nothing,d') -- started
      | B.null d = Just (Just t,"")-- ended
      | otherwise = Just (Just t,d')
      where
        (t,d) = B.break (== (c2w '/')) bs
        d' = if B.null d then "" else B.tail d

putHeader :: B.ByteString -> B.ByteString -> Put
putHeader k v = do
  putByteString k
  putByteString ": "
  putByteString v
  putByteString "\r\n"

mkBadRequestResponse :: B.ByteString -> B.ByteString
mkBadRequestResponse msg = BL.toStrict $ runPut $ do
  putByteString "HTTP/1.1 400 Bad Request\r\n"
  putHeader "Content-Length" $ BC.pack $ show $ B.length msg
  putHeader "Content-Type" "text/plain;charset=utf-8"
  putHeader "Connection" "Closed"
  putByteString "\r\n"
  putByteString msg
  
mkHandshakeResponse :: B.ByteString -> B.ByteString
mkHandshakeResponse wskey = BL.toStrict $ runPut $ do
  putByteString "HTTP/1.1 101 Switching Protocols\r\n"
  putHeader "Upgrade" "websocket"
  putHeader "Connection" "Upgrade"
  putHeader "Sec-WebSocket-Accept" $ calculateAccept wskey
  putHeader "Sec-WebSocket-Protocol" "crosscourse"
  putByteString "\r\n"
  where
    magicString = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    sha1hash = (convert :: Digest SHA1 -> B.ByteString) . hash
    calculateAccept = B64.encode . sha1hash . flip mappend magicString

wsHandshake :: Handle -> IO () -> IO ()
wsHandshake hdl success = parseWith chunk request "" >>= maybe badRequest f . maybeResult
    where
      f (Request "GET" _ "1.1",hdrs) = do
        let hdrs' = map (\(Header k vs) -> (BC.map toLower k,mconcat vs)) hdrs
        flushBody hdrs'
        maybe badRequest ((>> success) . B.hPut hdl . mkHandshakeResponse) $ do
          ensure "Upgrade"     $ lookup "connection"             hdrs'
          ensure "websocket"   $ lookup "upgrade"                hdrs'
          ensure "13"          $ lookup "sec-websocket-version"  hdrs'
          ensure "crosscourse" $ lookup "sec-websocket-protocol" hdrs'
          lookup "sec-websocket-key" hdrs'
      f _ = badRequest
      chunk = B.hGetSome hdl 4092
      flushBody hdrs = maybe (pure ()) (void . B.hGetNonBlocking hdl) $ do
        fmap fst . BC.readInt =<< lookup "content-length" hdrs
      badRequest = B.hPut hdl $ mkBadRequestResponse "invalid websocket handshake."
      ensure v = maybe Nothing (bool Nothing (Just ()) . (== v))