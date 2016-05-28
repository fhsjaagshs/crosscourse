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
  Handshake(..),
  mkBadRequestResponse,
  mkHandshakeResponse,
  readHandshake
)
where
  
import CrossCourse.WebSocket.RFC2616

import Control.Monad (when)
import Data.List
import Data.Maybe

import qualified Crypto.Hash.SHA1 as SHA1

import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Binary
import Data.Binary.Put
import Data.Attoparsec.ByteString

{-
TODO
- flush body after reading headers
- optimize & validate HTTP responses
-}

data Handshake = Handshake {
  handshakePathInfo :: [B.ByteString],
  handshakeWSKey :: B.ByteString
} deriving (Eq,Show)

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
  putHeader "Content-Length" $ BL.toStrict $ runPut $ put $ B.length msg
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
    calculateAccept = B64.encode . SHA1.hash . flip mappend magicString
    
-- TODO: flush body
readHandshake :: IO B.ByteString -> IO (Maybe Handshake)
readHandshake src = do
  mrq <- maybeResult <$> parseWith src request ""
  case mrq of
    Just (Request meth uri version,hdrs) -> do
      let hdrs' = map (\(Header k vs) -> (k,vs)) hdrs
      return $ case (meth,
                     version,
                     lookup "Connection" hdrs',
                     lookup "Upgrade" hdrs',
                     lookup "Sec-WebSocket-Key" hdrs',
                     lookup "Sec-WebSocket-Version" hdrs',
                     lookup "Sec-WebSocket-Protocol" hdrs'
                     ) of
                       ("GET",
                        "1.1",
                        Just ["Upgrade"],
                        Just ["websocket"],
                        Just [key],
                        Just ["13"],
                        Just ["crosscourse"]) ->
                        Just $ Handshake (splitPath uri) key
                       _ -> Nothing