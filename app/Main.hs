{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import qualified Network.AWS.SecretsManager as SM

import           Control.Lens               (view)
import           Control.Monad.Trans.AWS    (Credentials (Discover), Env,
                                             Region (Tokyo), newEnv, runAWST,
                                             runResourceT, send, within)


main :: IO ()
main = do
  env <- newEnv Discover
  ret <- getSecrets env "YOUR_SECRET_NAME" Tokyo -- Notice region 'Tokyo'. Change to yours.
  print $ fromJust ret


getSecrets :: Env -> Text -> Region-> IO (Maybe Text)
getSecrets awsEnv secretArn region =
  runResourceT . runAWST awsEnv . within region $ do
    resp <- send (SM.getSecretValue secretArn)
    return $ view SM.gsvrsSecretString resp
