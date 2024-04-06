module Logger (logger) where

import           Data.Text
import           Data.Time

logger :: Text -> IO ()
logger msg = do
  now <- formatTime defaultTimeLocale "%F - %T" <$> getCurrentTime
  Prelude.putStrLn $ mconcat ["[" , now, "] ", unpack msg]
