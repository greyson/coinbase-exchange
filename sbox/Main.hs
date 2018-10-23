{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8           as CBS
import           Data.Char                       (toUpper)
import           Data.Maybe
import           Data.Time
import           Data.UUID
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.WebSockets              as WS
import           System.Environment

import           Coinbase.Exchange
import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private
import           Coinbase.Exchange.Types.Socket

main :: IO ()
main = printSocket -- putStrLn "Use GHCi."

btc :: ProductId
btc = "BTC-USD"

start :: Maybe UTCTime
start = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

parseUseSandbox at = case map toUpper at of
  "TRUE"  -> Right Sandbox
  "FALSE" -> Right Live
  _other  -> Left $ "Sandbox configuration must be either TRUE or FALSE"

getEnvCBS = liftM CBS.pack . getEnv

withCoinbase :: (ExchangeConf -> IO a) -> IO a
withCoinbase act = do
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

  act (ExchangeConf mgr (Just tok) apiType)

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
