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

import Data.Maybe
import Data.List
import Data.Word
import Data.UUID
import Data.UUID.V4
import Data.Monoid
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
- implement 'Outgoing' serialization

Maybe:
- persistence (Postgres?,Web hook?)
  * chat grouping
  * messages
- clustering via frame forwarding
  * @type Logic = Auth -> Handle -> Pipe Message (Maybe (Handle,Message)) IO ()@
    * @Just (hdl,msg)@ means yes, this node can fulfill
    * @Nothing@ means this node cannot
-}

type Logic = Auth -> Handle -> Pipe Message (Maybe (Handle,Message)) IO ()
type Auth = MVar (Maybe UUID)

type HandleTable = H.CuckooHashTable UUID Handle
type ChatTable = H.CuckooHashTable UUID [UUID]

mkLogic :: IO Logic
mkLogic = logic <$> H.new <*> H.new

logic :: HandleTable -> ChatTable -> Logic
logic hdls chats = \auth hdl -> deserialize >-> logic' hdl auth hdls chats >-> serialize hdls

-- |Deserialize incoming messages into 'Incoming's.
deserialize :: Pipe Message Incoming IO ()
deserialize = do
  v@(Message msg isBinary) <- await
  lift $ print v
  when isBinary $ yield $ runGet readIncoming msg -- TODO: handle failure to parse 'Incoming's

-- |Serialize ('UUID','Outgoing') pairs into something recognizeable by the server logic.
serialize :: HandleTable -> Pipe (UUID,Outgoing) (Maybe (Handle,Message)) IO ()
serialize hdls = do
  (uuid,outgoing) <- await
  let msg = Message (runPut $ serializeOutgoing outgoing) True

  -- FIXME: remove handles from this when closing the connection
  dest <- lift $ H.lookup hdls uuid
  maybe (yield Nothing) (yield . Just . (,msg)) dest
        
logic' :: Handle -> Auth -> HandleTable -> ChatTable -> Pipe Incoming (UUID,Outgoing) IO ()
logic' hdl authmv hdls chats = do
  authv <- lift $ readMVar authmv
  msg <- await
  lift $ print msg
  f authv msg
  where
    mapMChat cuuid f = (lift $ H.lookup chats cuuid) >>= mapM_ f . fromMaybe []
    f Nothing (IAuthenticate user) = do
      lift $ do
        print user
        swapMVar authmv (Just user)
        H.insert hdls user hdl
      yield (user,OAuthSuccess)
    f (Just auth) (IAuthenticate user)
      | auth == user = yield (user,OAuthSuccess)
      | otherwise = f Nothing (IAuthenticate user)
    f (Just auth) (IStartTyping c) = mapMChat c $ yield . (,OStartTyping auth c)
    f (Just auth) (IStopTyping c) = mapMChat c $ yield . (,OStopTyping auth c)
    f (Just auth) (IMessage c k d) = mapMChat c $ yield . (,OMessage auth c k d)
    f (Just auth) (ICreateChat users) = do
      let users' = nub $ auth:users
      cuuid <- lift $ nextRandom
      lift $ H.insert chats cuuid users'
      mapM_ (\u -> yield (u,OChatInclusion cuuid)) users'
    f Nothing _ = lift $ closeWebsocketCode
                    hdl
                    unauthedError
                    "attempted to do operation requiring authentication whilst unauthenticated."
    
unauthedError :: Word16
unauthedError = 100

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
  deriving (Eq,Show)
  
readIncoming :: Get Incoming
readIncoming = getWord8 >>= f
  where
    f 0 = IAuthenticate <$> get
    f 1 = IStartTyping <$> get
    f 2 = IStopTyping <$> get
    f 3 = IMessage <$> get <*> getWord8 <*> (getWord64be >>= getByteString . fromIntegral)
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
  deriving (Eq,Show)

-- TODO: implement
serializeOutgoing :: Outgoing -> Put
serializeOutgoing OAuthSuccess = putByteString "auth success"
serializeOutgoing (OStartTyping user chat) = putByteString "start typing"
serializeOutgoing (OStopTyping user chat) = putByteString "stop typing"
serializeOutgoing (OMessage sender chat kind data_) = putByteString $ "message: " <> data_
serializeOutgoing (OChatInclusion chat) = putByteString $ "chat inclusion"