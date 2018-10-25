{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Coinbase.Exchange.Types
    ( Token
    , key
    , secret
    , passphrase
    , mkToken

    , ExchangeConf (..)

    -- * Internals
    , ApiType (..)
    , I.Request

    -- * Core types
    , Aggregate
    , ClientOrderId(ClientOrderId)
    , CoinScientific
    , Cost
    , CurrencyId(CurrencyId)
    , Empty
    , OrderId
    , OrderStatus(..)
    , OrderType
    , Price(..)
    , ProductId(..)
    , Quantity(..)
    , Reason
    , Sequence
    , Side(..)
    , TradeId

    -- * Market data types

    , Book(..)
    , Candle(..)
    , Currency(..)
    , ExchangeTime(..)
    , Product(..)
    , Stats(..)
    , Tick(..)
    , Trade(..)

    -- * Private data types

    , Account(..)
    , AccountId
    , BitcoinWallet(..)
    , BTCTransferId
    , BTCTransferReq(..)
    , BTCTransferResponse(..)
    , CoinbaseAccount(..)
    , CoinbaseAccountId
    , CryptoWallet(..)
    , CryptoWithdrawal(..)
    , CryptoWithdrawalResp(..)
    , Entry(..)
    , EntryDetails(..)
    , EntryId
    , EntryType(..)
    , Fill(..)
    , Hold(..)
    , HoldId
    , Liquidity(..)
    , NewOrder(..)
    , Order(..)
    , OrderCancelAfter(..)
    , OrderConfirmation(..)
    , OrderContingency(..)
    , ReportFormat(..)
    , ReportId
    , ReportInfo(..)
    , ReportParams(..)
    , ReportRequest(..)
    , ReportStatus(..)
    , ReportType(..)
    , SelfTrade(..)
    , TransferId
    , TransferToCoinbase(..)
    , TransferToCoinbaseResponse(..)

    ) where

import           Coinbase.Exchange.Internal   as I
import           Control.Applicative
import           Control.Concurrent.MVar
import           Data.ByteString
import qualified Data.ByteString.Base64       as Base64
import           Data.Data
import           Data.Text                    (Text)
import           Data.Time.Clock              (UTCTime)
import           GHC.Generics
import           Network.HTTP.Client

import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.MarketData
import           Coinbase.Exchange.Types.Private

--
-- Exchange configuration

data ExchangeConf
    = ExchangeConf
        { manager   :: Manager
        , authToken :: Maybe Token
        , apiType   :: ApiType
        , rateLimit :: MVar [UTCTime]
        }

--
-- Credentials token

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
