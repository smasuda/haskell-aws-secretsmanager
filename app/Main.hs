module Main where

import           AWSSM                       (getSecretValueByKey)
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.CachedIO (cachedIO)
import           Control.Monad               (forM_)
import           Data.String.Conversions     (cs)
import           System.Environment          (getArgs)
main :: IO ()
-- ^Loop is to check how many times the library hits AWS secret manager.
main = do
  [key, name] <- map cs <$> getArgs
  cachedSecret <- cachedIO 5 (getSecretValueByKey key name)
  forM_ [1..10] $ \_-> do
    cachedSecret >>= putStrLn
    threadDelay (1000*1000)
