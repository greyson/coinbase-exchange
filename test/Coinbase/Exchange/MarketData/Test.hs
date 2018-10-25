{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.MarketData.Test
    ( tests
    ) where

import           Control.Monad.IO.Class
import           Data.Aeson                   (ToJSON, FromJSON)
import           Data.Either                  (isRight)
import           Data.Time
import           Test.Tasty
import           Test.Tasty.HUnit

import           Coinbase.Exchange

--------------------------------
-- NOTE: ["case_parse" test cases]
--
-- The 'case_parse' function does NOT test that the API responses are
-- parsed correctly For example, the price can be "$240" be parsed as
-- "$420" and the test will succeed.  The function only tests that the
-- parser did not fail and returned *a value* Whether the value
-- returned is the correct one, that's a different matter, and
-- 'case-parse' is NOT testing that.
--
--------------------------------
tests :: ExchangeConf -> TestTree
tests conf = testGroup "MarketData Parse"
  [ case_parse conf "getProducts"      $ getProducts
  , case_parse conf "getTopOfBook"     $ getTopOfBook     defProduct
  , case_parse conf "getTop50OfBook"   $ getTop50OfBook   defProduct
  , case_parse conf "getOrderBook"     $ getOrderBook     defProduct
  , case_parse conf "getProductTicker" $ getProductTicker defProduct
  , case_parse conf "getTrades"        $ getTrades        defProduct
  , case_parse conf "getHistory"       $ getHistory       defProduct Nothing Nothing (Just 3600)
  , case_parse conf "getCurrencies"    $ getCurrencies
  , case_parse conf "getExchangeTime"  $ getExchangeTime
  ]

-- See NOTE: ["case_parse" test cases]
case_parse :: (ToJSON a, FromJSON b, Show b)
           => ExchangeConf -> String -> Request a b -> TestTree
case_parse conf l r = testCase l $ do
        v <- coinbaseRest conf r
        assertBool ("Failed to parse: " ++ show v) (isRight v)

defProduct :: ProductId
defProduct = ProductId "BTC-USD"

-- The date range below works well to provide data for the getHistory call in the sandboxed environment
-- The previous range yielded an empty list with no data when connected to the sandbox.
defStart :: Maybe UTCTime
defStart = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2018-10-01T20:22:37+0000"

defEnd :: Maybe UTCTime
defEnd = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2018-10-07T21:22:37+0000"
