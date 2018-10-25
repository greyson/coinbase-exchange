{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad                   (forever)
import           Data.Aeson
import           Data.Maybe
import           Data.Time
import           Data.UUID
import qualified Network.WebSockets              as WS
import           System.Environment

import           Coinbase.Exchange
import           Coinbase.Exchange.Socket

main :: IO ()
main = printSocket -- putStrLn "Use GHCi."

btc :: ProductId
btc = "BTC-USD"

start :: Maybe UTCTime
start = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

printSocket :: IO ()
printSocket = subscribe Live [btc] $ \conn -> do
        putStrLn "Connected."
        _ <- forkIO $ forever $ do
            ds <- WS.receiveData conn
            let res = eitherDecode ds
            case res :: Either String ExchangeMessage of
                Left er -> print er
                Right v -> print v
        _ <- forever $ threadDelay (1000000 * 60)
        return ()
