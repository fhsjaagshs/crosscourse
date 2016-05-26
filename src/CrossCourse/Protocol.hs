module CrossCourse.Protocol
(
  Command(..)
)
where
  
import Data.Word

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
  
import Data.UUID.V4
import qualified Data.ByteString as B
  
data Command
  = StartedTyping {
    cmdStartedTypingUser :: UUID,
    cmdStartedTypingChat :: UUID
  }
  | StoppedTyping {
    cmdStoppedTypingUser :: UUID,
    cmdStoppedTypingChat :: UUID
  }
  | SendMessage {
    cmdSentMessageUser :: UUID,
    cmdSentMessageChat :: UUID,
  --   cmdSentMessageTimestamp :: UTCTime, -- TODO: timestamp
    cmdSentMessageKind :: Word8,
    cmdSentMessageData :: B.ByteString
  }
  | CreateChat {
    cmdCreateChatUser :: UUID,
    cmdCreateChatChat :: Maybe UUID
  }
  | JoinChat {
    cmdJoinChatUser :: UUID,
    cmdJoinChatChat :: UUID
  }
  | LeaveChat {
    cmdLeaveChatUser :: UUID,
    cmdLeaveChatChat :: UUID
  }
  | Error {
    cmdErrorCode :: Word8,
    cmdErrorMessage :: B.ByteString
  }
  
instance Binary Command where
  get = do
    opcode <- getWord8
    case opcode of
      0 -> StartedTyping <$> get <*> get
      1 -> StoppedTyping <$> get <*> get
      2 -> SendMessage <$> get <*> get <*> getWord8 <*> get
      3 -> CreateChat <$> get <*> get
      4 -> JoinChat <$> get <*> get
      5 -> LeaveChat <$> get <*> get
      6 -> Error <$> getWord8 <*> get
  put (StartedTyping u c) = putWord8 0 >> put u >> put c
  put (StoppedTyping u c) = putWord8 1 >> put u >> put c
  put (SendMessage u c k d) = putWord8 2 >> put u >> put c >> putWord8 k >> putByteString d
  put (CreateChat u c) = putWord8 3 >> put u >> put c
  put (JoinChat u c) = putWord8 4 >> put u >> put c
  put (LeaveChat u c) = putWord8 5 >> put u >> put c
  put (Error c m) = putWord8 6 >> putWord8 c >> putByteString m
    