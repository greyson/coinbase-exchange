{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Private
    ( getAccountList
    , getAccount
    , getAccountLedger
    , getAccountHolds

    , createOrder
    , cancelOrder
    , cancelAllOrders
    , getOrderList
    , getOrder

    , getFills

    , createTransfer
    , createCryptoWithdrawal

    , createReport
    , getReportStatus

    , module Coinbase.Exchange.Types.Private
    ) where

import           Data.Char
import           Data.List                       hiding (delete)
import qualified Data.Text                       as T
import           Data.UUID

import           Coinbase.Exchange.Internal
import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private

import           Network.HTTP.Types (StdMethod(..))

get :: String -> Maybe a -> Request a b
get = request endpointRest GET True

post :: String -> Maybe a -> Request a b
post = request endpointRest POST True

delete :: String -> Maybe a -> Request a b
delete = request endpointRest DELETE True

-- Accounts

getAccountList :: Request () [Account]
getAccountList = get "/accounts" Nothing

getAccount :: AccountId -> Request () Account
getAccount i = get ("/accounts/" ++ urlParam i) Nothing

getAccountLedger :: AccountId -> Request () [Entry]
getAccountLedger i = get ("/accounts/" ++ urlParam i ++ "/ledger") Nothing

getAccountHolds :: AccountId -> Request () [Hold]
getAccountHolds i = get ("/accounts/" ++ urlParam i ++ "/holds") Nothing

-- Orders

createOrder :: NewOrder -> Request NewOrder OrderConfirmation
createOrder o = post "/orders" (Just o)

cancelOrder :: OrderId -> Request () [OrderId]
cancelOrder o = delete ("/orders/" ++ urlParam o) Nothing

cancelAllOrders :: Maybe ProductId -> Request () [OrderId]
cancelAllOrders prodId = delete "/orders" Nothing
  .&? ("product_id", prodId)

getOrderList :: [OrderStatus] -> Request () [Order]
getOrderList [] =
  getOrderList [ Open, Pending, Active ]
getOrderList os =
  foldr (\s r -> r .& ("status", s)) (get "/orders" Nothing) os

getOrder :: OrderId -> Request () Order
getOrder o = get ("/orders/" ++ urlParam o) Nothing

-- Fills

getFills :: Maybe OrderId -> Maybe ProductId -> Request () [Fill]
getFills moid mpid = get "/fills" Nothing
  .&? (  "order_id", moid)
  .&? ("product_id", mpid)

-- Transfers

createTransfer :: TransferToCoinbase
               -> Request TransferToCoinbase TransferToCoinbaseResponse
createTransfer = post "/transfers" . Just

createCryptoWithdrawal :: CryptoWithdrawal
                       -> Request CryptoWithdrawal CryptoWithdrawalResp
createCryptoWithdrawal = post "/withdrawals/crypto" . Just

-- Reports

createReport :: ReportRequest -> Request ReportRequest ReportInfo
createReport = post "/reports" . Just

getReportStatus :: ReportId -> Request () ReportInfo
getReportStatus r = get  ("/reports/" ++ urlParam r) Nothing
