{-# LANGUAGE OverloadedStrings #-}
module CrossCourse.HTTP
(
  socketApp,
  startWebSocket
)
where
  
import CrossCourse.WebSocket
  
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings,runSettingsSocket,Port)
-- import Network.Wai.Handler.WarpTLS (tlsSettings,runTLSSocket,TLSSettings(..),OnInsecure(..))
import Network.Wai.Handler.Warp.Internal
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

import Data.Bool
import Data.Maybe
import Data.Either
import Data.Monoid

import Data.Binary
import Data.Bits

import Pipes
import qualified Pipes.Prelude as P

import Control.Concurrent (myThreadId,killThread)

import Network.Socket (sClose,withSocketsDo,Socket)
import Data.Streaming.Network (bindPortTCP)
import Control.Exception (bracket)

-- TODO: pipe-based stream parsing

withTCPSocket :: Port -> (Socket -> IO ()) -> IO ()
withTCPSocket port f = withSocketsDo $ bracket (bindPortTCP port "*4") sClose f

-- TODO: secure websockets
-- |Start a websocket WAI app.
startWebSocket :: Port -- ^ port to run server on
               -> IO ()
startWebSocket port = withTCPSocket port run
  where settings = defaultSettings { settingsPort = port }
        run sock = runSettingsSocket settings sock socketApp

socketApp :: Application
socketApp req respond 
  | requestMethod req == methodGet = handshake >> respond runSocket
  | otherwise = respond $ responseLBS status400 [] "bad request"
  where
    remoteSockAddr = remoteHost req
    headers = requestHeaders req
    magicString = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    
    ensure :: Eq a => a -> Maybe a -> Maybe a
    ensure v ma = maybe Nothing (bool Nothing ma . (v ==)) ma
    
    handshake = respond $ fromMaybe (responseLBS status400 [] "bad request") $ do
      ensure "websocket" $ lookup "Upgrade" headers
      ensure "Upgrade" $ lookup "Connection" headers
      ensure "crosscourse" $ lookup "Sec-WebSocket-Protocol" headers
      ensure "13" $ lookup "Sec-WebSocket-Version" headers
      websocketKey <- lookup "Sec-WebSocket-Key" headers
      return $ responseLBS
                status101
                [("Upgrade","websocket"),
                 ("Connection","Upgrade"),
                 ("Sec-WebSocket-Protocol","crosscourse"),
                 ("Sec-WebSocket-Accept", B64.encode $ SHA1.hash $ websocketKey <> magicString)] 
                ""
                
    runSocket = flip responseRaw fallback $ \src sink -> runEffect $ mkpipe src sink
      where fallback = responseLBS status500 [] "server does not support websockets"
            mkpipe r w = readPipe r >-> parserPipe >-> writePipe w >-> consumer
      
readPipe :: IO B.ByteString -> Producer B.ByteString IO ()
readPipe src = do
  (lift src) >>= yield
  readPipe src
  
parserPipe :: Pipe B.ByteString Frame IO ()
parserPipe = do
  s <- await
  case parseFrame s of
    Left err -> lift $ print err
    Right v -> yield v
  parserPipe

-- TODO: implement actual payloads
-- data FrameType
--     = ContinuationFrame
--     | TextFrame
--     | BinaryFrame
--     deriving (Eq, Show)

writePipe :: (B.ByteString -> IO ()) -> Pipe Frame (BL.ByteString,Bool) IO ()
writePipe sink = (await >>= f) >> writePipe sink
  where
    f (Frame _ _ _ _ PongFrame _) = return ()
    f (Frame _ o tw tr PingFrame a) = lift $ sink $ BL.toStrict $ B.toLazyByteString $ encodeFrame $ Frame False o tw tr PongFrame a
    f (Frame _ _ _ _ CloseFrame _) = lift (myThreadId >>= killThread)
    f (Frame _ _ _ _ _ payload) = yield (payload,False) -- TODO: continuations

  -- (Frame aheader apayload) <- await
  -- (Frame bheader bpayload) <- await
  -- case (frameHdrType aheader,frameHdrType bheader) of
  --   ()
  --
  
consumer :: Consumer (BL.ByteString,Bool) IO ()
consumer = do
  (txt,_) <- await
  lift $ print txt
  consumer
      