{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Coinbase.Exchange.Types.MarketData where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad                (mzero)
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Data
import           Data.Hashable
import           Data.Int
import           Data.String
import           Data.Text                    (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Vector                  as V
import           Data.Word
import           GHC.Generics

import           Coinbase.Exchange.Internal
import           Coinbase.Exchange.Types.Core

--
-- Book: Order Book

data Book a
    = Book
        { bookSequence :: Sequence
        , bookBids     :: [BookItem a]
        , bookAsks     :: [BookItem a]
        }
    deriving (Show, Data, Typeable, Generic)

instance (NFData a) => NFData (Book a)
instance (ToJSON a) => ToJSON (Book a) where
    toJSON = genericToJSON coinbaseAesonOptions
instance (FromJSON a) => FromJSON (Book a) where
    parseJSON = genericParseJSON coinbaseAesonOptions

data BookItem a = BookItem Price Size a
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance (NFData a) => NFData (BookItem a)
instance (ToJSON a) => ToJSON (BookItem a) where
    toJSON = genericToJSON defaultOptions
instance (FromJSON a) => FromJSON (BookItem a) where
    parseJSON = genericParseJSON defaultOptions

--
-- Candle: Historic Rates

data Candle = Candle
  { cUTCTime :: UTCTime
  , cLow :: Price
  , cHigh :: Price
  , cOpen :: Price
  , cClose :: Price
  , cVolume :: Quantity
  }
  deriving (Show, Data, Typeable, Generic)

instance NFData Candle
instance FromJSON Candle where
    parseJSON = fmap candle . parseJSON
      where candle (t, lo, hi, op, cl, v) =
              Candle (posixSecondsToUTCTime $ fromIntegral (t :: Int64))
              (Price lo) (Price hi) (Price op) (Price cl)
              (Quantity v)

--
-- Currency: Exchange Currencies

data Currency
    = Currency
        { curId      :: CurrencyId
        , curName    :: Text
        , curMinSize :: CoinScientific
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Currency
instance ToJSON Currency where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Currency where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- ExchangeTime: Exchange Time

data ExchangeTime
    = ExchangeTime
        { timeIso   :: UTCTime
        , timeEpoch :: Double
        }
    deriving (Show, Data, Typeable, Generic)

instance ToJSON ExchangeTime where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON ExchangeTime where
    parseJSON = genericParseJSON coinbaseAesonOptions
--
-- Product: Products

data Product
    = Product
        { prodId             :: ProductId
        , prodBaseCurrency   :: CurrencyId
        , prodQuoteCurrency  :: CurrencyId
        , prodBaseMinSize    :: CoinScientific
        , prodBaseMaxSize    :: CoinScientific
        , prodQuoteIncrement :: CoinScientific
        , prodDisplayName    :: Text
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Product
instance ToJSON Product where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Product where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- Stats: Product Stats

data Stats
    = Stats
        { statsOpen   :: Price
        , statsHigh   :: Price
        , statsLow    :: Price
        , statsClose  :: Price
        , statsVolume :: Quantity
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Stats
instance ToJSON Stats where
    toJSON Stats{..} = object
        [ "open"    .= statsOpen
        , "high"    .= statsHigh
        , "low"     .= statsLow
        , "close"   .= statsClose
        , "volume"  .= statsVolume
        ]
instance FromJSON Stats where
    parseJSON = withObject "Stats" $ \o ->
      Stats <$> o .: "open"
            <*> o .: "high"
            <*> o .: "low"
            <*> o .: "close"
            <*> o .: "volume"

-- Tick: Product Ticker

data Tick
    = Tick
        { tickTradeId :: Word64
        , tickPrice   :: Price
        , tickSize    :: Size
        , tickTime    :: Maybe UTCTime
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Tick
instance ToJSON Tick where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Tick where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- Trade: Product Trades

data Trade
    = Trade
        { tradeTime    :: UTCTime
        , tradeTradeId :: TradeId
        , tradePrice   :: Price
        , tradeSize    :: Size
        , tradeSide    :: Side
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Trade
instance ToJSON Trade where
    toJSON Trade{..} = object [ "time"      .= tradeTime
                              , "trade_id"  .= tradeTradeId
                              , "price"     .= tradePrice
                              , "size"      .= tradeSize
                              , "side"      .= tradeSide
                              ]
instance FromJSON Trade where
  parseJSON = withObject "Trade" $ \m ->
    Trade <$> m .: "time"
          <*> m .: "trade_id"
          <*> m .: "price"
          <*> m .: "size"
          <*> m .: "side"
