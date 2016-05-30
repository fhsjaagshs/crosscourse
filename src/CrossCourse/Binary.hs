module CrossCourse.Binary
(
  encode',
  runGetWith
)
where
  
import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

encode' :: Binary a => a -> B.ByteString
encode' = BL.toStrict . encode

-- |The 'Data.Binary' equivalent of Attoparsec's 'parseWith'.      
runGetWith :: (Monad m, Binary a)
           => Get a
           -> B.ByteString -- ^ data to feed in that was leftover from previous runs
           -> m B.ByteString -- ^ action to read in data
           -> m (Either String (B.ByteString,a)) -- Either err (remaining,result)
runGetWith get carry src = d $ pushChunk (runGetIncremental get) carry
  where d (Partial f) = src >>= d . f . ensureLength
        d (Done remaining _ result) = return $ Right (remaining,result) -- parse remaining (accum ++ [result])
        d (Fail _ _ err) = return $ Left err
        ensureLength bs
          | B.null bs = Nothing
          | otherwise = Just bs