{-# LANGUAGE OverloadedStrings,TupleSections #-}
module CrossCourse.Logic
(
  Logic,
  Auth,
  mkLogic
)
where
  
import CrossCourse.WebSocket

import Pipes
import Control.Concurrent hiding (yield)

import Network.Socket

import Data.List
import Data.Word
import Data.UUID
import Data.UUID.V4
import qualified Data.ByteString as B

import Control.Monad

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.HashTable.IO as H
  
{-
TODO
- rewrite the SocketTable to use Handles
- finish designing/implementing "crosscourse" protocol
- maybe message persistence (Postgres?,Web hook?)
- push notifications
-}

type Logic = Auth -> Pipe (Socket,Message) (Socket,Message) IO ()
type Auth = MVar (Maybe UUID)

type SocketTable = H.CuckooHashTable UUID (MVar Socket)
type ChatTable = H.CuckooHashTable UUID [UUID]
type ReverseSocketTable = H.CuckooHashTable Socket UUID

mkLogic :: IO Logic
mkLogic = logic <$> H.new <*> H.new

logic :: SocketTable -> ChatTable -> Logic
logic sockets chats auth = deserialize >-> logic' auth chats >-> serialize sockets

-- |Deserialize incoming messages into 'Incoming's.
deserialize :: Pipe (a,Message) (a,Incoming) IO ()
deserialize = do
  (v,Message msg isBinary) <- await
  when isBinary $ yield (v,runGet readIncoming msg) -- TODO: handle failure to parse messages

-- |Serialize ('UUID','Outgoing') pairs into something recognizeable by the server logic.
serialize :: SocketTable -> Pipe (UUID,Outgoing) (Socket,Message) IO ()
serialize sockets = do
  (uuid,outgoing) <- await
  
  when (outgoing == OAuthSuccess) $ do
    return ()
    -- TODO: insert into socktable
    
  sockmv <- lift $ H.lookup sockets uuid
  
  when (outgoing == ODeAuthSuccess) $ do
    lift $ H.delete sockets uuid
    -- TODO: close socket?
  
  yield (sock,Message (runPut $ serializeOutgoing outgoing) True)
        
logic' :: Auth -> ChatTable -> Pipe (Socket,Incoming) (UUID,Outgoing) IO ()
logic' authmv chats = do
  authv <- lift $ readMVar authmv
  (sock,msg) <- await
  f sock authv msg
  where
    mapMChat cuuid f = H.lookup chats cuuid >>= mapM_ f
    f s Nothing (IAuthenticate user) = do
      lift $ swapMVar authmv user
      yield (user,OAuthSuccess)
    f _ (Just auth) (IAuthenticate user)
      | auth == user = yield (user,OAuthSuccess)
      | otherwise = yield (user,OError alreadyAuthedError)
    f _ (Just auth) IDeAuthenticate = yield (auth,ODeAuthSuccess)
    f _ (Just auth) (IStartTyping c) = mapMChat c $ yield . (,OStartTyping auth c)
    f _ (Just auth) (IStopTyping c) = mapMChat c $ yield . (,OStopTyping auth c)
    f _ (Just auth) (IMessage c k d) = mapMChat c $ yield . (,OMessage auth c k d)
    f _ (Just auth) (ICreateChat users) = do
      let users' = nub $ auth:users
      cuuid <- lift $ nextRandom
      lift $ H.insert chats cuuid users'
      mapM_ (\u -> yield (u,OChatInclusion cuuid)) users'
    f _ Nothing _ = return ()-- yield (???,OError unauthedError)
    
  
alreadyAuthedError :: Word8
alreadyAuthedError = 100

unauthedError :: Word8
unauthedError = 101

data Incoming
  = IAuthenticate {
      iAuthUUID :: UUID
    }
  | IDeAuthenticate
  | IStartTyping {
      iStartTypingChat :: UUID
    }
  | IStopTyping {
      iStopTypingChat :: UUID
    }
  | IMessage {
      iMessageChat :: UUID,
      iMessageKind :: Word8,
      iMessageData :: B.ByteString
    }
  | ICreateChat {
      iCreateChatUsers :: [UUID]
    }
  | IUnsupported

readIncoming :: Get Incoming
readIncoming = getWord8 >>= f
  where
    f 0 = IAuthenticate <$> get
    f 1 = return IDeAuthenticate
    f 2 = IStartTyping <$> get
    f 3 = IMessage <$> get <*> getWord8 <*> getByteString
    f 4 = ICreateChat <$> getList
    f _ = return IUnsupported
    getList :: Binary a => Get [a]
    getList = go =<< getWord8
      where
        go left
          | left <= 0 = pure []
          | otherwise = (:) <$> get <*> go (left-1)
      

data Outgoing
  = OAuthSuccess
  | ODeAuthSuccess
  | OStartTyping {
      oStartTypingUser :: UUID,
      oStartTypingChat :: UUID
    }
  | OStopTyping {
      oStopTypingUser :: UUID,
      oStopTypingChat :: UUID
    }
  | OMessage {
      oMessageSender :: UUID,
      oMessageChat :: UUID,
      oMessageKind :: Word8,
      oMessageData :: B.ByteString
    }
  | OChatInclusion {
      oChatInclusionChat :: UUID
    }
  | OError {
      oErrorCode :: Word8
    }
    
serializeOutgoing :: Outgoing -> Put
serializeOutgoing OAuthSuccess = return ()
serializeOutgoing ODeAuthSuccess = return ()
serializeOutgoing (OStartTyping user chat) = return ()
serializeOutgoing (OStopTyping user chat) = return ()
serializeOutgoing (OMessage sender chat kind data_) = return ()
serializeOutgoing (OChatInclusion chat) = return ()
serializeOutgoing (OError code) = return ()