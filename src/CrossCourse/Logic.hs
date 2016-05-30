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
logic hdls chats = \auth hdl -> deserialize hdl >-> logic' hdl auth hdls chats >-> serialize hdls

-- |Deserialize incoming messages into 'Incoming's.
deserialize :: Handle -> Pipe Message Incoming IO ()
deserialize hdl = do
  Message msg isBinary <- await
  if not isBinary
    then lift $ closeWebsocketCode hdl invalidMessageError "utf8 message received"
    else case runGetOrFail getIncoming msg of
      Left (_,_,err) -> lift $ closeWebsocketCode hdl invalidMessageError err
      Right (_,_,v) -> yield v

-- |Serialize ('UUID','Outgoing') pairs into something recognizeable by the server logic.
serialize :: HandleTable -> Pipe (UUID,Outgoing) (Maybe (Handle,Message)) IO ()
serialize hdls = do
  (uuid,outgoing) <- await
  let msg = Message (runPut $ putOutgoing outgoing) True

  -- FIXME: remove handles from this when closing the connection
  dest <- lift $ H.lookup hdls uuid
  maybe (yield Nothing) (yield . Just . (,msg)) dest
        
logic' :: Handle -> Auth -> HandleTable -> ChatTable -> Pipe Incoming (UUID,Outgoing) IO ()
logic' hdl authmv hdls chats = (lift $ readMVar authmv) >>= (await >>=) . f
  where
    mapMChat cuuid g = (lift $ H.lookup chats cuuid) >>= mapM_ g . fromMaybe []
    f _ (IAuthenticate user) = do
      lift $ do
        old <- swapMVar authmv (Just user)
        maybe (pure ()) (H.delete hdls) old
        H.insert hdls user hdl
      yield (user,OAuthSuccess)
    f (Just auth) (IStartTyping c) = mapMChat c $ yield . (,OStartTyping auth c)
    f (Just auth) (IStopTyping c) = mapMChat c $ yield . (,OStopTyping auth c)
    f (Just auth) (IMessage c k d) = mapMChat c $ yield . (,OMessage auth c k d)
    f (Just auth) (ICreateChat users) = do
      let users' = nub $ auth:users
      cuuid <- lift $ nextRandom
      lift $ H.insert chats cuuid users'
      mapM_ (yield . (,OChatInclusion cuuid)) users'
    f Nothing _ = lift $ closeWebsocketCode hdl unauthedError "not authenticated"
    
unauthedError :: Word16
unauthedError = 101

invalidMessageError :: Word16
invalidMessageError = 100

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
  
getIncoming :: Get Incoming
getIncoming = getWord8 >>= f
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

putOutgoing :: Outgoing -> Put
putOutgoing OAuthSuccess = putWord8 0
putOutgoing (OStartTyping u c) = putWord8 1 >> put u >> put c
putOutgoing (OStopTyping u c) = putWord8 2 >> put u >> put c
putOutgoing (OMessage s c k d) = putWord8 3 >> put s >> put c >> put k >> put d
putOutgoing (OChatInclusion c) = putWord8 4 >> put c