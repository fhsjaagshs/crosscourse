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
import System.IO

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
- Errors should always close the connection
    * prevents uncooperative clients from doing Bad Things™
- Clean up implementation and make it compile/work
- push notifications

Maybe:
- persistence (Postgres?,Web hook?)
  * chat grouping
  * messages
- clustering via frame forwarding
  * @type Logic = Auth -> Handle -> Pipe Message (Maybe (Handle,Message)) IO ()@
    * @Just (hdl,msg)@ means yes, this node can fulfill
    * @Nothing@ means this node cannot

-}

type Logic = Auth -> Handle -> Pipe Message (Handle,Message) IO ()
type Auth = MVar (Maybe UUID)

type HandleTable = H.CuckooHashTable UUID Handle
type ChatTable = H.CuckooHashTable UUID [UUID]

mkLogic :: IO Logic
mkLogic = logic <$> H.new <*> H.new

logic :: HandleTable -> ChatTable -> Logic
logic hdls chats auth hdl = deserialize >-> logic' auth hdls chats >-> serialize hdls

-- |Deserialize incoming messages into 'Incoming's.
deserialize :: Pipe Message Incoming IO ()
deserialize = do
  Message msg isBinary <- await
  when isBinary $ yield $ runGet readIncoming msg -- TODO: handle failure to parse 'Incoming's

-- |Serialize ('UUID','Outgoing') pairs into something recognizeable by the server logic.
serialize :: HandleTable -> Pipe (UUID,Outgoing) (Handle,Message) IO ()
serialize hdls = do
  (uuid,outgoing) <- await
  let msg = Message (runPut $ serializeOutgoing outgoing) True

  -- FIXME: remove handles from this when closing the connection
  dest <- lift $ H.lookup hdls uuid
  yield (dest,msg)
        
logic' :: Handle -> Auth -> HandleTable -> ChatTable -> Pipe Incoming (UUID,Outgoing) IO ()
logic' hdl authmv hdls chats = do
  authv <- lift $ readMVar authmv
  msg <- await
  f sock authv msg
  where
    mapMChat cuuid f = H.lookup chats cuuid >>= mapM_ f
    f Nothing (IAuthenticate user) = do
      lift $ do
        swapMVar authmv user
        H.insert hdls user hdl
      yield (user,OAuthSuccess)
    f (Just auth) (IAuthenticate user)
      | auth == user = yield (user,OAuthSuccess)
      | otherwise = yield (user,OError alreadyAuthedError)
    f (Just auth) (IStartTyping c) = mapMChat c $ yield . (,OStartTyping auth c)
    f (Just auth) (IStopTyping c) = mapMChat c $ yield . (,OStopTyping auth c)
    f (Just auth) (IMessage c k d) = mapMChat c $ yield . (,OMessage auth c k d)
    f (Just auth) (ICreateChat users) = do
      let users' = nub $ auth:users
      cuuid <- lift $ nextRandom
      lift $ H.insert chats cuuid users'
      mapM_ (\u -> yield (u,OChatInclusion cuuid)) users'
    f Nothing _ = return ()-- FIXME: close the connection!
    
  
alreadyAuthedError :: Word8
alreadyAuthedError = 100

unauthedError :: Word8
unauthedError = 101

data Incoming
  = IAuthenticate {
      iAuthUUID :: UUID
    }
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

readIncoming :: Get Incoming
readIncoming = getWord8 >>= f
  where
    f 0 = IAuthenticate <$> get
    f 1 = IStartTyping <$> get
    f 2 = IStopTyping <$> get
    f 3 = IMessage <$> get <*> getWord8 <*> getByteString
    f 4 = ICreateChat <$> getList
    f _ = fail "unsupported message"
    getList :: Binary a => Get [a]
    getList = go =<< getWord8
      where
        go left
          | left <= 0 = pure []
          | otherwise = (:) <$> get <*> go (left-1)
      
data Outgoing
  = OAuthSuccess
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

serializeOutgoing :: Outgoing -> Put
serializeOutgoing OAuthSuccess = return ()
serializeOutgoing (OStartTyping user chat) = return ()
serializeOutgoing (OStopTyping user chat) = return ()
serializeOutgoing (OMessage sender chat kind data_) = return ()
serializeOutgoing (OChatInclusion chat) = return ()