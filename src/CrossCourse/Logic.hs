{-# LANGUAGE OverloadedStrings,TupleSections #-}
module CrossCourse.Logic
(
  Logic,
  Auth,
  mkLogic
)
where
  
import CrossCourse.WebSocket
import CrossCourse.Binary

import Pipes
import Control.Concurrent hiding (yield)

-- import Network.Socket
import System.IO

import Data.Maybe
import Data.List
import Data.Word
import Data.UUID
import Data.UUID.V4
-- import Data.Monoid
import qualified Data.ByteString as B

-- import Control.Monad

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.HashTable.IO as H
  
{-
TODO
- de-authenticate when closing ** server-wide
- use a monad to handle auth, handletables, and the current handle.
  - could be used to fail and provide a reason, to be used by CrossCourse.Server
- push notifications

Maybe:
- persistence (Postgres?,Web hook?)
  * chat grouping
  * messages
- clustering via frame forwarding
  * @type Logic = Auth -> Handle -> Pipe Message (Maybe (Handle,Message)) IO ()@
    * @Just (hdl,msg)@ means yes, this node can fulfill
    * @Nothing@ means this node cannot
- clustering in 'messageLogic' (forward message to cluster)
-}

type Logic = Auth -> Handle -> Consumer Message IO ()
type Auth = MVar (Maybe UUID)

type HandleTable = H.CuckooHashTable UUID Handle
type ChatTable = H.CuckooHashTable UUID [UUID]

mkLogic :: IO Logic
mkLogic = logic <$> H.new <*> H.new

logic :: HandleTable -> ChatTable -> Logic
logic hdls chats = \mv hdl -> deserialize hdl >-> logic' hdl mv hdls chats >-> serialize hdls >-> sendMessage

-- |authentication
auth :: HandleTable -> Auth -> Maybe (UUID,Handle) -> IO ()
auth hdls authmv v = do
  old <- swapMVar authmv (fst <$> v)
  maybe (pure ()) (H.delete hdls) old
  maybe (pure ()) (uncurry (H.insert hdls)) v

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
  dest            <- lift $ H.lookup hdls uuid
  let msg = Message (runPut $ putOutgoing outgoing) True
  maybe (yield Nothing) (yield . Just . (,msg)) dest
  
sendMessage :: Consumer (Maybe (Handle,Message)) IO ()
sendMessage = await >>= f
  where f (Just (dest,msg)) = lift $ B.hPut dest $ encode' msg
        f _ = return ()

logic' :: Handle -> Auth -> HandleTable -> ChatTable -> Pipe Incoming (UUID,Outgoing) IO ()
logic' hdl authmv hdls chats = (lift $ readMVar authmv) >>= (await >>=) . f
  where
    mapMChat cuuid g = (lift $ H.lookup chats cuuid) >>= mapM_ g . fromMaybe []
    f _ (IAuthenticate user) = do
      lift $ auth hdls authmv (Just (user,hdl))
      yield (user,OAuthSuccess user)
    f (Just a) (IStartTyping c) = mapMChat c $ yield . (,OStartTyping a c)
    f (Just a) (IStopTyping c) = mapMChat c $ yield . (,OStopTyping a c)
    f (Just a) (IMessage c k d) = mapMChat c $ yield . (,OMessage a c k d)
    f (Just a) (ICreateChat us) = do
      let us' = nub $ a:us
      cuuid <- lift $ nextRandom
      lift $ H.insert chats cuuid us'
      mapM_ (yield . (,OChatInclusion cuuid)) us'
    f Nothing _ = lift $ closeWebsocketCode hdl unauthedError "not authenticated"
    
unauthedError :: Word16
unauthedError = 101

invalidMessageError :: Word16
invalidMessageError = 100

data Incoming
  = IAuthenticate
      UUID -- ^ uuid to authenticate
  | IStartTyping
      UUID -- ^ chat UUID
  | IStopTyping
      UUID -- ^ chat UUID
  | IMessage 
      UUID -- ^ chat uuid
      Word8 -- ^ message kind TODO: define this
      B.ByteString -- ^ message body
  | ICreateChat
      [UUID] -- ^ relevant user UUIDs
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
      UUID -- ^ authenticated user
  | OStartTyping
      UUID -- ^ typing user
      UUID -- ^ chat uuid he's typing in
  | OStopTyping
      UUID -- ^ typing user
      UUID -- ^ chat uuid he's typing in
  | OMessage
      UUID -- ^ UUID of sender
      UUID -- ^ chat uuid
      Word8 -- ^ message kind (see 'IMessage' constructor)
      B.ByteString -- ^ message payload
  | OChatInclusion
      UUID -- ^ relevant chat UUID
  deriving (Eq,Show)

putOutgoing :: Outgoing -> Put
putOutgoing (OAuthSuccess u) = putWord8 0 >> put u
putOutgoing (OStartTyping u c) = putWord8 1 >> put u >> put c
putOutgoing (OStopTyping u c) = putWord8 2 >> put u >> put c
putOutgoing (OMessage s c k d) = putWord8 3 >> put s >> put c >> put k >> put d
putOutgoing (OChatInclusion c) = putWord8 4 >> put c