{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Coinbase.Exchange
  ( ExchangeFailure (..)
  , I.Request
  , coinbaseRest
  , module Coinbase.Exchange.Types.MarketData
  , module Coinbase.Exchange.Types.Private
  ) where

import qualified Coinbase.Exchange.Internal   as I
import           Coinbase.Exchange.Types.MarketData
import           Coinbase.Exchange.Types.Private
import           Coinbase.Exchange.Rest
import           Control.Exception            (Exception, IOException)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Attoparsec.ByteString
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Data                    (Data, Typeable)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock              (getCurrentTime)
import           GHC.Generics                 (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Types

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
                     deriving (Show, Data, Typeable, Generic)

newtype ShowRequest = ShowRequest Request
  deriving (Typeable, Generic)

instance Data ShowRequest

instance Show ShowRequest where
  show (ShowRequest req) = "Request "
    ++ "{ method = " ++ show (method req)
    ++ ", path = " ++ show (path req)
    ++ ", queryString = " ++ show (queryString req)
    ++ " }"


instance Exception ExchangeFailure
