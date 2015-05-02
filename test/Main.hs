module Main where

import           Control.Monad
import qualified Data.ByteString.Char8             as CBS
import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit
import           System.Environment
import           Test.Tasty

import           Coinbase.Exchange.Types

import qualified Coinbase.Exchange.MarketData.Test as MarketData

main :: IO ()
main = do
        mgr     <- newManager tlsManagerSettings
        tKey    <- liftM CBS.pack $ getEnv "COINBASE_KEY"
        tSecret <- liftM CBS.pack $ getEnv "COINBASE_SECRET"
        tPass   <- liftM CBS.pack $ getEnv "COINBASE_PASSPHRASE"
        defaultMain (tests $ ExchangeConf mgr (Just $ Token tKey tSecret tPass))

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Tests"
        [ MarketData.tests conf
        ]