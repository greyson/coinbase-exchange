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

    , module Coinbase.Exchange.Types.MarketData
    ) where

import           Coinbase.Exchange.Internal
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
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

newtype BrokenUTCTime = BrokenUTCTime { realTime :: UTCTime }
instance UrlEncode BrokenUTCTime where
  urlParam (BrokenUTCTime t) = formatTime defaultTimeLocale "%FT%T." t

type StartTime  = UTCTime
type EndTime    = UTCTime
type Scale      = Int

getHistory :: ProductId -> Maybe StartTime -> Maybe EndTime -> Maybe Scale
           -> Request () [Candle]
getHistory p start end scale = get ("/products/" ++ urlParam p ++ "/candles")
  .&? (      "start", BrokenUTCTime <$> start)
  .&? (        "end", BrokenUTCTime <$> end)
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
