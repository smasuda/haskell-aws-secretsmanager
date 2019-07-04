module AWSSM (getSecretValueByKey) where

import           Control.Lens               (view)
import           Control.Monad.Trans.AWS    (Credentials (Discover), Env,
                                             Region (Tokyo), newEnv, runAWST,
                                             runResourceT, send, within)
import           Data.Aeson                 (Value, decode)
import           Data.Aeson.Types           (Parser, parseMaybe, withObject,
                                             (.:))
import           Data.Maybe                 (fromJust)
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import           Debug.Trace                (trace)
import qualified Network.AWS.SecretsManager as SM

getSecretValueByKey::Text -> Text -> IO String
-- ^throws exception if secretName nor secretKey isn't available.
getSecretValueByKey secretName secretKey = do
    env <- newEnv Discover
    ret <- getSecrets env secretName Tokyo -- Notice region 'Tokyo'. Change to yours.
    return $ do
        let decoded = ret >>= \x-> decode $ cs x ::Maybe Value
        fromJust $ parseMaybe (parseSecrets secretKey) =<< decoded

parseSecrets::Text -> Value -> Parser String
parseSecrets key = withObject "obj" $ \o-> o .: key

getSecrets :: Env -> Text -> Region -> IO (Maybe Text)
getSecrets awsEnv secretName region =
  runResourceT . runAWST awsEnv . within region $ trace "[accessing secret]" $ do
    resp <- send (SM.getSecretValue secretName) -- could throw exception.
    return $ view SM.gsvrsSecretString resp
