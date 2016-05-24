module CrossCourse.Protocol
(
 --  Command(..)
)
where
  
-- import Data.Int
-- import Data.Word
-- import Data.Binary
--
-- data Command
--   = StartedTyping Int64 -- an integer representing the ID of the user that started typing
--   | StoppedTyping Int64 -- an integer representing the ID of the user that stopped typing
--   | SendMessage Int64 Integer ByteString
--   | InvalidCommand
  
-- instance Binary Command where
--   get = do
--     opcode <- getWord8
--     case opcode of
--       1 -> -- started typing
--       2 -> -- stopped typing
--       3 -> -- send message
--       _ -> return InvalidCommand