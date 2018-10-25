{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Coinbase.Exchange.MarketData
    ( getProducts
    , getTopOfBook
    , getTop50OfBook
    , getOrderBook

    , getProductTicker
    , getTrades
    , getHistory
    , getStats

    , getCurrencies
    , getExchangeTime
    ) where

import           Coinbase.Exchange.Internal
import           Data.List
import qualified Data.Text                          as T
import           Data.Time

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format                   (defaultTimeLocale)
#else
import           System.Locale                      (defaultTimeLocale)
#endif

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.MarketData

import           Network.HTTP.Types (StdMethod(..))

-- this is the only request type we'll need for this module
get path = request endpointRest GET False path Nothing

-- Products

getProducts :: Request () [Product]
getProducts = get "/products"

-- Order Book

getTopOfBook :: ProductId -> Request () (Book Aggregate)
getTopOfBook p = get ("/products/" ++ urlParam p ++ "/book?level=1")

getTop50OfBook :: ProductId -> Request () (Book Aggregate)
getTop50OfBook p = get ("/products/" ++ urlParam p ++ "/book?level=2")

getOrderBook :: ProductId -> Request () (Book OrderId)
getOrderBook p = get ("/products/" ++ urlParam p ++ "/book?level=3")

-- Product Ticker

getProductTicker :: ProductId -> Request () Tick
getProductTicker p = get ("/products/" ++ urlParam p ++ "/ticker")

-- Product Trades

getTrades :: ProductId -> Request () [Trade]
getTrades p = get ("/products/" ++ urlParam p ++ "/trades")

-- Historic Rates (Candles)

-- | Currently Broken:
-- coinbase api doesn't return valid ISO 8601 dates for this route.

newtype ISOTime = ISOTime { fixUTCTime :: UTCTime }
instance UrlEncode ISOTime where
  urlParam (ISOTime t) = formatTime defaultTimeLocale "%FT%TZ" t

getHistory :: ProductId
           -> Maybe UTCTime -- ^ Start time
           -> Maybe UTCTime -- ^ End time
           -> Maybe Int     -- ^ Interval
           -> Request () [Candle]
getHistory p start end scale = get ("/products/" ++ urlParam p ++ "/candles")
  .&? (      "start", ISOTime <$> start)
  .&? (        "end", ISOTime <$> end)
  .&? ("granularity", scale)

-- Product Stats

getStats :: ProductId -> Request () Stats
getStats p = get ("/products/" ++ urlParam p ++ "/stats")

-- Exchange Currencies

getCurrencies :: Request () [Currency]
getCurrencies = get "/currencies"

-- Exchange Time

getExchangeTime :: Request () ExchangeTime
getExchangeTime = get "/time"
