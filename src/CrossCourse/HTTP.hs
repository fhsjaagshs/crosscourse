{-# LANGUAGE OverloadedStrings #-}
module CrossCourse.HTTP
(
  socketApp,
  startWebSocket,
  Message(..)
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

data Message = Message {
  messagePayload :: !BL.ByteString,
  messageIsBinary :: !Bool
} deriving (Eq,Show)

type Handler = Message -> (Message -> IO ()) -> IO ()

withTCPSocket :: Port -> (Socket -> IO ()) -> IO ()
withTCPSocket port f = withSocketsDo $ bracket (bindPortTCP port "*4") sClose f

-- TODO: secure websockets
-- |Start a websocket WAI app.
startWebSocket :: Handler -- ^ function that handles messages
               -> Port -- ^ port to run server on
               -> IO ()
startWebSocket handler port = withTCPSocket port run
  where settings = defaultSettings { settingsPort = port }
        run sock = runSettingsSocket settings sock (socketApp handler)

socketApp :: Handler -> Application
socketApp handler req respond
  | requestMethod req == methodGet = handshake >> respond runSocket
  | otherwise = respond $ responseLBS status400 [] "bad request"
  where
    remoteSockAddr = remoteHost req
    headers = requestHeaders req
    magicString = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    
    ensure :: Eq a => a -> Maybe a -> Maybe a
    ensure v ma = maybe Nothing (bool Nothing ma . (v ==)) ma
    
    -- TODO: make sure this handshake is valid. Something about nonces in the response
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
            mkpipe r w = readPipe r >-> parserPipe >-> writePipe w handler
      
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

writePipe :: (B.ByteString -> IO ()) -> Handler -> Consumer Frame IO ()
writePipe sink handler = (await >>= f) >> writePipe sink handler
  where
    f (Frame _ _ _ _ PongFrame _) = return ()
    f (Frame _ o tw tr PingFrame a) = do
      lift $ putStrLn "pinging"
      lift $ sendFrame $ Frame True o tw tr PongFrame a
    f c@(Frame _ _ _ _ CloseFrame _) = do
      lift $ putStrLn "closing"
      lift $ sendFrame c
      lift (myThreadId >>= killThread)
    -- TODO: continuations
    f (Frame _ _ _ _ TextFrame payload) = lift $ handler (Message payload False) sendMessage
    f (Frame _ _ _ _ BinaryFrame payload) = lift $ handler (Message payload True) sendMessage
    sendFrame = sink . BL.toStrict . encodeFrame
    sendMessage (Message p True) = sendFrame $ Frame True False False False BinaryFrame p
    sendMessage (Message p False) = sendFrame $ Frame True False False False TextFrame p
      