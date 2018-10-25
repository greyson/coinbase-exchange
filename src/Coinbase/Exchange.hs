{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Coinbase.Exchange
  ( ExchangeFailure (..)
  , coinbaseRest

  , withCoinbaseEnv
  , module Coinbase.Exchange.Types
  , module Coinbase.Exchange.MarketData
  , module Coinbase.Exchange.Private
  , module Coinbase.Exchange.Rest
  ) where

import qualified Coinbase.Exchange.Internal   as I
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Rest
import           Control.Concurrent.MVar      (newMVar)
import           Control.Exception            (Exception, IOException)
import           Control.Monad                (liftM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Attoparsec.ByteString
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as CBS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Char                    (toUpper)
import           Data.Data                    (Data(..), Typeable)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock              (getCurrentTime)
import           GHC.Generics                 (Generic)
import           Network.HTTP.Client          as HTTP
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Types
import           System.Environment           (getEnv)

coinbaseRest :: ( ToJSON a, FromJSON b )
             => ExchangeConf -> I.Request a b
             -> IO (Either ExchangeFailure b)
coinbaseRest conf r = do
  req <- makeRequest conf r <$> liftIO getCurrentTime
  withResponse req (manager conf) $ \res -> do
    let breader = responseBody res
    case responseStatus res of
      s | s == status200 || (s == created201) -> runExceptT $
          ExceptT (goodJSON <$> parseWith (brRead breader) value' BS.empty)
          >>= decodeCoinbase
      otherwise ->
        Left . ApiFailure (ShowRequest req) . T.decodeUtf8 <$> (brRead breader)
goodJSON (Done _ a) = Right $! a
goodJSON (Fail rem _ s) = Left $ ParseFailure rem s
goodJSON _other = Left $ ParseFailure BS.empty "Insufficient input"

decodeCoinbase v = case fromJSON v of
  (Success a) -> return $! a
  (Error e) -> throwE $ DecodeFailure v e
--
-- Infrastructure for executing requests the conduit way.

data ExchangeFailure = ParseFailure BS.ByteString String
                     | DecodeFailure Value String
                     | ApiFailure ShowRequest Text
                     | AuthenticationRequiredFailure Text
                     | AuthenticationRequiresByteStrings
                     deriving (Show, Typeable, Generic)

instance Exception ExchangeFailure

-- | A simple wrapper around requests to allow simple showing.
newtype ShowRequest = ShowRequest HTTP.Request
  deriving (Typeable, Generic)

instance Show ShowRequest where
  show (ShowRequest req) = "Request "
    ++ "{ method = " ++ show (method req)
    ++ ", path = " ++ show (path req)
    ++ ", queryString = " ++ show (queryString req)
    ++ " }"

--
-- Use a simple configuration from environment variables

withCoinbaseEnv :: (ExchangeConf -> IO a) -> IO a
withCoinbaseEnv act = do
  -- Parse the api type from the environment
  apiType <- getEnv "GDAX_SANDBOX"
    >>= either error return . parseUseSandbox

  -- Parse the security token from the environment
  tok     <- mkToken <$> (getEnvCBS "GDAX_KEY")
                     <*> (getEnvCBS "GDAX_SECRET")
                     <*> (getEnvCBS "GDAX_PASSPHRASE")
    >>= either (error . show) return

  -- Create a new HTTP manager (TLS settings)
  mgr <- newManager tlsManagerSettings

  -- Create the empty list signifying our last request times.
  limit <- newMVar []

  act (ExchangeConf mgr (Just tok) apiType limit)

parseUseSandbox at = case map toUpper at of
  "TRUE"  -> Right Sandbox
  "FALSE" -> Right Live
  _other  -> Left $ "Sandbox configuration must be either TRUE or FALSE"

getEnvCBS = liftM CBS.pack . getEnv
