{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Monad
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment
import           Test.Tasty

import           Coinbase.Exchange

import qualified Coinbase.Exchange.MarketData.Test as MarketData
import qualified Coinbase.Exchange.Private.Test    as Private
import qualified Coinbase.Exchange.Socket.Test     as Socket

main :: IO ()
main = withCoinbaseEnv $ \conf -> defaultMain (tests conf)

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Tests"
        [ MarketData.tests conf
        , Private.tests    conf
        , Socket.tests conf (ProductId "ETH-BTC")
        ]
