{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Coinbase.Exchange.Types
    ( ApiType (..)
    , Endpoint
    , Path

    , website
    , endpointRest
    , endpointSocket
    , endpointRealRest

    , Token
    , key
    , secret
    , passphrase
    , mkToken

    , ExchangeConf (..)
    ) where

import           Coinbase.Exchange.Internal
import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString
import qualified Data.ByteString.Base64       as Base64
import           Data.Data
import           Data.Text                    (Text)
import           GHC.Generics
import           Network.HTTP.Conduit

-- API URLs

type Endpoint = String
type Path     = String

website :: Endpoint
website = "https://public.sandbox.gdax.com"

-- Monad Stack

data Token
    = Token
        { key        :: ByteString
        , secret     :: ByteString
        , passphrase :: ByteString
        }

mkToken :: ByteString -- ^ Key
        -> ByteString -- ^ Secret
        -> ByteString -- ^ Passphrase
        -> Either String Token
mkToken k s p = Token k <$> Base64.decode s <*> pure p

data ExchangeConf
    = ExchangeConf
        { manager   :: Manager
        , authToken :: Maybe Token
        , apiType   :: ApiType
        }
