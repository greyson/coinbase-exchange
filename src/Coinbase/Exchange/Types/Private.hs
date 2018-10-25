{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Coinbase.Exchange.Types.Private where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import           Data.Hashable
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.UUID
import           Data.Word
import           GHC.Generics

import           Coinbase.Exchange.Internal
import           Coinbase.Exchange.Types.Core

--
-- Account

data Account
    = Account
        { accId        :: AccountId
        , accBalance   :: CoinScientific
        , accHold      :: CoinScientific
        , accAvailable :: CoinScientific
        , accCurrency  :: CurrencyId
        }
    deriving (Show, Eq, Data, Typeable, Generic)

instance NFData Account
instance ToJSON Account where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Account where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- AccountId

newtype AccountId = AccountId { unAccountId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)
instance UrlEncode AccountId where
  urlParam = toString . unAccountId

--
-- BitcoinWallet

newtype BitcoinWallet = FromBTCAddress { btcAddress :: String }
      deriving (Show, Data, Typeable, Generic, ToJSON, FromJSON)
instance NFData BitcoinWallet

--
-- BTCTransferId

newtype BTCTransferId = BTCTransferId { getBtcTransferId :: UUID }
    deriving ( Eq, Ord, Show, Read, Data, Typeable
             , Generic, NFData, FromJSON, ToJSON)

--
-- BTCTransferReq

data BTCTransferReq
    = SendBitcoin
        { sendAmount    :: Size
        , bitcoinWallet :: BitcoinWallet
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData BTCTransferReq
instance ToJSON BTCTransferReq where
    toJSON SendBitcoin {..} = object
        [ "type"     .= ("send" :: Text)
        , "currency" .= ("BTC"  :: Text)
        , "to"       .= btcAddress bitcoinWallet
        , "amount"   .= sendAmount
        ]

--
-- BTCTransferResponse

data BTCTransferResponse = BTCTransferResponse
        { sendId :: BTCTransferId
        -- FIX ME! and other stuff I'm going to ignore.
        } deriving (Eq, Data, Show, Generic, Typeable)

instance NFData BTCTransferResponse
instance FromJSON BTCTransferResponse where
    parseJSON = withObject "BTCTransferResponse" $ \m ->
      -- FIX ME! I should factor this out of all responses from Coinbase
      BTCTransferResponse <$> (m .: "data" >>= (.: "id"))

--
-- CoinbaseAccount

data CoinbaseAccount =
    CoinbaseAccount
        { cbAccID      :: CoinbaseAccountId
        , resourcePath :: String
        , primary      :: Bool
        , name         :: String
        , btcBalance   :: Size
        }
    deriving (Show, Data, Typeable, Generic)

instance FromJSON CoinbaseAccount where
  parseJSON = withObject "CoinbaseAccount" $ \m -> do
    -- FIX ME! I should factor this out of all responses from Coinbase
    transferData <- m .:? "data"
    case transferData of
      Nothing -> mzero
      Just da -> CoinbaseAccount
          <$> da .: "id"
          <*> da .: "resource_path"
          <*> da .: "primary"
          <*> da .: "name"
          <*> (do
                  btcBalance <- da .: "balance"
                  case btcBalance of
                      Object b -> b .: "amount"
                      _        -> mzero
              )

--
-- CoinbaseAccountId

newtype CoinbaseAccountId = CoinbaseAccountId { unCoinbaseAccountId :: UUID }
    deriving ( Eq, Ord, Show, Read, Data, Typeable
             , Generic, NFData, FromJSON, ToJSON)

--
-- CryptoWallet

data CryptoWallet
    = BTCWallet BitcoinWallet deriving (Show, Data, Typeable, Generic)
--  | To Do: add other...
--  | ... possibilities here later

instance NFData CryptoWallet
instance ToJSON CryptoWallet where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON CryptoWallet where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- CryptoWithdrawal

data CryptoWithdrawal
    = Withdrawal
        { wdAmount        :: Size
        , wdCurrency      :: CurrencyId
        , wdCryptoAddress :: CryptoWallet
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData CryptoWithdrawal
instance ToJSON CryptoWithdrawal where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON CryptoWithdrawal where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- CrypoWithdrawalResp

data CryptoWithdrawalResp
    = WithdrawalResp
        { wdrId       :: TransferId
        , wdrAmount   :: Size
        , wdrCurrency :: CurrencyId
        } deriving (Eq, Show, Generic, Typeable)

instance NFData CryptoWithdrawalResp
instance ToJSON CryptoWithdrawalResp where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON CryptoWithdrawalResp where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- Entry

data Entry
    = Entry
        { entryId        :: EntryId
        , entryCreatedAt :: UTCTime
        , entryAmount    :: CoinScientific
        , entryBalance   :: CoinScientific
        , entryType      :: EntryType
        , entryDetails   :: EntryDetails
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Entry
instance ToJSON Entry where
    toJSON Entry{..} = object [ "id"         .= entryId
                              , "created_at" .= entryCreatedAt
                              , "amount"     .= entryAmount
                              , "balance"    .= entryBalance
                              , "type"       .= entryType
                              , "details"    .= entryDetails
                              ]
instance FromJSON Entry where
    parseJSON (Object m) = Entry
        <$> m .: "id"
        <*> m .: "created_at"
        <*> m .: "amount"
        <*> m .: "balance"
        <*> m .: "type"
        <*> m .: "details"
    parseJSON _ = mzero

--
-- EntryDetails

data EntryDetails
    = EntryDetails
        { detailOrderId   :: Maybe OrderId
        , detailTradeId   :: Maybe TradeId
        , detailProductId :: Maybe ProductId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData EntryDetails
instance ToJSON EntryDetails where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON EntryDetails where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- EntryId

newtype EntryId = EntryId { unEntryId :: Word64 }
    deriving (Eq, Ord, Num, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

--
-- EntryType

data EntryType
    = Match
    | Fee
    | Transfer
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData EntryType
instance Hashable EntryType
instance ToJSON EntryType where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }
instance FromJSON EntryType where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower }

--
-- Fill

data Fill
    = Fill
        { fillTradeId   :: TradeId
        , fillProductId :: ProductId
        , fillPrice     :: Price
        , fillSize      :: Size
        , fillOrderId   :: OrderId
        , fillCreatedAt :: UTCTime
        , fillLiquidity :: Liquidity
        , fillFee       :: Price
        , fillSettled   :: Bool
        , fillSide      :: Side
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Fill
instance ToJSON Fill where
    toJSON Fill{..} = object
        [ "trade_id"    .= fillTradeId
        , "product_id"  .= fillProductId
        , "price"       .= fillPrice
        , "size"        .= fillSize
        , "order_id"    .= fillOrderId
        , "created_at"  .= fillCreatedAt
        , "liquidity"   .= fillLiquidity
        , "fee"         .= fillFee
        , "settled"     .= fillSettled
        , "side"        .= fillSide
        ]
instance FromJSON Fill where
  parseJSON = withObject "Fill" $ \m -> Fill
        <$> m .: "trade_id"
        <*> m .: "product_id"
        <*> m .: "price"
        <*> m .: "size"
        <*> m .: "order_id"
        <*> m .: "created_at"
        <*> m .: "liquidity"
        <*> m .: "fee"
        <*> m .: "settled"
        <*> m .: "side"

--
-- Hold

data Hold
    = OrderHold
        { holdId        :: HoldId
        , holdAccountId :: AccountId
        , holdCreatedAt :: UTCTime
        , holdUpdatedAt :: UTCTime
        , holdAmount    :: CoinScientific
        , holdOrderRef  :: OrderId
        }
    | TransferHold
        { holdId          :: HoldId
        , holdAccountId   :: AccountId
        , holdCreatedAt   :: UTCTime
        , holdUpdatedAt   :: UTCTime
        , holdAmount      :: CoinScientific
        , holdTransferRef :: TransferId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Hold
instance ToJSON Hold where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Hold where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- HoldId

newtype HoldId = HoldId { unHoldId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

--
-- Liquidity

data Liquidity
    = Maker
    | Taker
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData Liquidity
instance Hashable Liquidity
instance ToJSON Liquidity where
    toJSON Maker = String "M"
    toJSON Taker = String "T"
instance FromJSON Liquidity where
    parseJSON (String "M") = return Maker
    parseJSON (String "T") = return Taker
    parseJSON _            = mzero

--
-- NewOrder

data NewOrder
  = NewLimitOrder
    { noProductId   :: ProductId
    , noSide        :: Side
    , noSelfTrade   :: SelfTrade
    , noClientOid   :: Maybe ClientOrderId
      ---
    , noPrice       :: Price
    , noSize        :: Quantity
    , noTimeInForce :: OrderContingency
    , noCancelAfter :: Maybe OrderCancelAfter
    , noPostOnly    :: Bool
    }
  | NewMarketOrder
    { noProductId :: ProductId
    , noSide      :: Side
    , noSelfTrade :: SelfTrade
    , noClientOid :: Maybe ClientOrderId
      ---
    , noSizeAndOrFunds  :: Either Quantity (Maybe Quantity, Cost)
    }
  | NewStopOrder
    { noProductId :: ProductId
    , noSide      :: Side
    , noSelfTrade :: SelfTrade
    , noClientOid :: Maybe ClientOrderId
      ---
    , noPrice     :: Price
    , noSizeAndOrFunds  :: Either Quantity (Maybe Quantity, Cost)
    }
  deriving (Show, Data, Typeable, Generic)

instance NFData NewOrder
instance ToJSON NewOrder where
  toJSON NewLimitOrder{..} = object
    ([ "type" .= ("limit" :: Text)
     , "product_id"    .= noProductId
     , "side"          .= noSide
     , "stp"           .= noSelfTrade
     , "price"         .= noPrice
     , "size"          .= noSize
     , "time_in_force" .= noTimeInForce
     , "post_only"     .= noPostOnly
     ] ++ clientID ++ cancelAfter )
    where
      clientID = case noClientOid of
                   Just cid -> [ "client_oid" .= cid ]
                   Nothing  -> []
      cancelAfter = case noCancelAfter of
                      Just time -> [ "cancel_after" .= time ]
                      Nothing   -> []

  toJSON NewMarketOrder{..} = object
    ([ "type" .= ("market" :: Text)
     , "product_id"    .= noProductId
     , "side"          .= noSide
     , "stp"           .= noSelfTrade
     ] ++ catMaybes addl )
    where
      addl = [ ("client_oid" .=) <$> noClientOid ]
             ++ either (\s -> [Just $ "size" .= s])
                       (\(s,f) -> [ ("size" .=) <$> s, Just $ "funds" .= f])
                       noSizeAndOrFunds
  toJSON NewStopOrder{..} = object
    ([ "type" .= ("stop" :: Text)
     , "product_id"    .= noProductId
     , "side"          .= noSide
     , "stp"           .= noSelfTrade
     , "price"         .= noPrice
     ] ++ catMaybes addl )
     where
      addl = [ ("client_oid" .=) <$> noClientOid ]
             ++ either (\s -> [Just $ "size" .= s])
                       (\(s,f) -> [ ("size" .=) <$> s, Just $ "funds" .= f])
                       noSizeAndOrFunds

--
-- Order

data Order
    = LimitOrder
        { orderId         :: OrderId
        , orderProductId  :: ProductId
        , orderStatus     :: OrderStatus
        , orderSelfTrade  :: SelfTrade
        , orderSettled    :: Bool
        , orderSide       :: Side
        , orderCreatedAt  :: UTCTime
        , orderFilledSize :: Maybe Quantity
        , orderFilledFees :: Maybe Price
        , orderDoneAt     :: Maybe UTCTime
        , orderDoneReason :: Maybe Reason

        , orderPrice      :: Price
        , orderSize       :: Quantity
        , orderTimeInForce:: OrderContingency
        , orderCancelAfter:: Maybe OrderCancelAfter
        , orderPostOnly   :: Bool
        }
    | MarketOrder
        { orderId         :: OrderId
        , orderProductId  :: ProductId
        , orderStatus     :: OrderStatus
        , orderSelfTrade  :: SelfTrade
        , orderSettled    :: Bool
        , orderSide       :: Side
        , orderCreatedAt  :: UTCTime
        , orderFilledSize :: Maybe Quantity
        , orderFilledFees :: Maybe Price
        , orderDoneAt     :: Maybe UTCTime
        , orderDoneReason :: Maybe Reason

        , orderSizeAndOrFunds  :: Either Quantity (Maybe Quantity, Cost)
        }
    | StopOrder
        { orderId         :: OrderId
        , orderProductId  :: ProductId
        , orderStatus     :: OrderStatus
        , orderSelfTrade  :: SelfTrade
        , orderSettled    :: Bool
        , orderSide       :: Side
        , orderCreatedAt  :: UTCTime
        , orderFilledSize :: Maybe Quantity
        , orderFilledFees :: Maybe Price
        , orderDoneAt     :: Maybe UTCTime
        , orderDoneReason :: Maybe Reason

        , orderPrice      :: Price
        , orderSizeAndOrFunds  :: Either Quantity (Maybe Quantity, Cost)
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Order
instance ToJSON Order where
    toJSON LimitOrder{..} = object
        [ "type"          .= ("limit" :: Text)
        , "id"            .= orderId
        , "product_id"    .= orderProductId
        , "status"        .= orderStatus
        , "stp"           .= orderSelfTrade
        , "settled"       .= orderSettled
        , "side"          .= orderSide
        , "created_at"    .= orderCreatedAt
        , "filled_size"   .= orderFilledSize
        , "filled_fees"   .= orderFilledFees
        , "done_at"       .= orderDoneAt
        , "done_reason"   .= orderDoneReason

        , "price"         .= orderPrice
        , "size"          .= orderSize
        , "time_in_force" .= orderTimeInForce
        , "cancel_after"  .= orderCancelAfter
        , "post_only"     .= orderPostOnly
        ]
    toJSON MarketOrder{..} = object
       ([ "type" .= ("market" :: Text)
        , "id"            .= orderId
        , "product_id"    .= orderProductId
        , "status"        .= orderStatus
        , "stp"           .= orderSelfTrade
        , "settled"       .= orderSettled
        , "side"          .= orderSide
        , "created_at"    .= orderCreatedAt
        , "filled_size"   .= orderFilledSize
        , "filled_fees"   .= orderFilledFees
        , "done_at"       .= orderDoneAt
        , "done_reason"   .= orderDoneReason
        ] ++ size ++ funds )
            where (size,funds) = case orderSizeAndOrFunds of
                        Left  s -> (["size" .= s],[])
                        Right (ms,f) -> case ms of
                                    Nothing -> ( []            , ["funds" .= f] )
                                    Just s' -> ( ["size" .= s'], ["funds" .= f] )
    toJSON StopOrder{..} = object
       ([ "type" .= ("market" :: Text)
        , "id"            .= orderId
        , "product_id"    .= orderProductId
        , "status"        .= orderStatus
        , "stp"           .= orderSelfTrade
        , "settled"       .= orderSettled
        , "side"          .= orderSide
        , "created_at"    .= orderCreatedAt
        , "filled_size"   .= orderFilledSize
        , "filled_fees"   .= orderFilledFees
        , "done_at"       .= orderDoneAt
        , "done_reason"   .= orderDoneReason

        , "price"         .= orderPrice
        ] ++ size ++ funds )
            where (size,funds) = case orderSizeAndOrFunds of
                        Left  s -> (["size" .= s],[])
                        Right (ms,f) -> case ms of
                                    Nothing -> ( []            , ["funds" .= f] )
                                    Just s' -> ( ["size" .= s'], ["funds" .= f] )


instance FromJSON Order where
    parseJSON (Object m) = do
        ordertype <- m .: "type"
        case (ordertype :: String) of
            "limit" -> LimitOrder
                <$> m .: "id"
                <*> m .: "product_id"
                <*> m .: "status"
                <*> m .:? "stp" .!= DecreaseAndCancel
                <*> m .: "settled"
                <*> m .: "side"
                <*> m .: "created_at"
                <*> m .:? "filled_size"
                <*> m .:? "filled_fees"
                <*> m .:? "done_at"
                <*> m .:? "done_reason"
                <*> m .: "price"
                <*> m .: "size"
                <*> m .:? "time_in_force" .!= GoodTillCanceled -- older orders don't seem to have this field
                <*> m .:? "cancel_after"
                <*> m .: "post_only"

            "market" -> MarketOrder
                <$> m .: "id"
                <*> m .: "product_id"
                <*> m .: "status"
                <*> m .: "stp"
                <*> m .: "settled"
                <*> m .: "side"
                <*> m .: "created_at"
                <*> m .:? "filled_size"
                <*> m .:? "filled_fees"
                <*> m .:? "done_at"
                <*> m .:? "done_reason"
                <*> (do
                        ms <- m .:? "size"
                        mf <- m .:? "funds"
                        case (ms,mf) of
                            (Nothing, Nothing) -> mzero
                            (Just s , Nothing) -> return $ Left  s
                            (Nothing, Just f ) -> return $ Right (Nothing, f)
                            (Just s , Just f ) -> return $ Right (Just s , f)
                            )

            "stop" -> StopOrder
                <$> m .: "id"
                <*> m .: "product_id"
                <*> m .: "status"
                <*> m .: "stp"
                <*> m .: "settled"
                <*> m .: "side"
                <*> m .: "created_at"
                <*> m .:? "filled_size"
                <*> m .:? "filled_fees"
                <*> m .:? "done_at"
                <*> m .:? "done_reason"
                <*> m .: "price"
                <*> (do
                        ms <- m .:? "size"
                        mf <- m .:? "funds"
                        case (ms,mf) of
                            (Nothing, Nothing) -> mzero
                            (Just s , Nothing) -> return $ Left  s
                            (Nothing, Just f ) -> return $ Right (Nothing, f)
                            (Just s , Just f ) -> return $ Right (Just s , f)
                            )
            _ -> mzero

    parseJSON _ = mzero

--
-- OrderCancelAfter

data OrderCancelAfter
    = Min
    | Hour
    | Day
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData   OrderCancelAfter
instance Hashable OrderCancelAfter

instance ToJSON OrderCancelAfter where
    toJSON Min                  = String "min"
    toJSON Hour                 = String "hour"
    toJSON Day                  = String "day"
instance FromJSON OrderCancelAfter where
    parseJSON (String "min")    = return Min
    parseJSON (String "hour")   = return Hour
    parseJSON (String "day")    = return Day
    parseJSON _ = mzero

--
-- OrderConfirmation

data OrderConfirmation
    = OrderConfirmation
        { ocId :: OrderId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData OrderConfirmation
instance ToJSON OrderConfirmation where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON OrderConfirmation where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- OrderContingency

data OrderContingency
    = GoodTillCanceled
    | GoodTillTime
    | ImmediateOrCancel
    | FillOrKill
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData   OrderContingency
instance Hashable OrderContingency

instance ToJSON OrderContingency where
  toJSON v = String $ case v of
    GoodTillCanceled  -> "GTC"
    GoodTillTime      -> "GTT"
    ImmediateOrCancel -> "IDC"
    FillOrKill        -> "FOK"
instance FromJSON OrderContingency where
  parseJSON = withText "OrderContingency" $ \t ->
    case t of
      "GTC" -> return GoodTillCanceled
      "GTT" -> return GoodTillTime
      "IOC" -> return ImmediateOrCancel
      "FOK" -> return FillOrKill
      other -> fail $ "Unknown OrderContingency: " ++ T.unpack other

--
-- ReportFormat

data ReportFormat
    = PDF
    | CSV
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData   ReportFormat
instance Hashable ReportFormat

instance ToJSON ReportFormat where
    toJSON PDF                  = String "pdf"
    toJSON CSV                  = String "csv"
instance FromJSON ReportFormat where
    parseJSON (String "pdf")    = return PDF
    parseJSON (String "csv")    = return CSV
    parseJSON _ = mzero

--
-- ReportId

newtype ReportId = ReportId { unReportId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, FromJSON, ToJSON)
instance UrlEncode ReportId where
  urlParam = toString . unReportId

--
-- ReportInfo

data ReportInfo
    = ReportInfo
        { reportId          :: ReportId
        , reportType        :: ReportType
        , reportStatus      :: ReportStatus
        , reportCreated     :: Maybe UTCTime
        , reportCompleted   :: Maybe UTCTime
        , reportExpires     :: Maybe UTCTime
        , reportUrl         :: Maybe String
        , reportParams      :: Maybe ReportParams
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData ReportInfo
instance ToJSON ReportInfo where
    toJSON ReportInfo{..} = object
       ([ "id"            .= reportId
        , "type"          .= reportType
        , "status"        .= reportStatus
        , "created_at"    .= reportCreated
        , "completed_at"  .= reportCompleted
        , "expires_at"    .= reportExpires
        , "file_url"      .= reportUrl
        , "params"        .= reportParams
        ])

instance FromJSON ReportInfo where
    parseJSON (Object m) = ReportInfo
            <$> m .:  "id"
            <*> m .:  "type"
            <*> m .:  "status"
            <*> m .:? "created_at"
            <*> m .:? "completed_at"
            <*> m .:? "expires_at"
            <*> m .:? "file_url"
            <*> m .:? "params"
    parseJSON _ = mzero

--
-- ReportParams

data ReportParams
    = ReportParams
        { reportStartDate       :: UTCTime
        , reportEndDate         :: UTCTime
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData ReportParams
instance ToJSON ReportParams where
    toJSON ReportParams{..} = object
       ([ "start_date"        .= reportStartDate
        , "end_date"          .= reportEndDate
        ])
instance FromJSON ReportParams where
    parseJSON (Object m) = ReportParams
            <$> m .:  "start_date"
            <*> m .:  "end_date"
    parseJSON _ = mzero

--
-- ReportRequest

data ReportRequest -- analgous to Transfer or NewOrder
    = ReportRequest
        { rrqType            :: ReportType
        , rrqStartDate       :: UTCTime
        , rrqEndDate         :: UTCTime
        , rrqProductId       :: ProductId
        , rrqAccountId       :: AccountId
        , rrqFormat          :: ReportFormat
        , rrqEmail           :: Maybe String
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData ReportRequest
instance ToJSON ReportRequest where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON ReportRequest where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- ReportStatus

data ReportStatus
    = ReportPending
    | ReportCreating
    | ReportReady
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData   ReportStatus
instance Hashable ReportStatus

instance ToJSON ReportStatus where
    toJSON ReportPending        = String "pending"
    toJSON ReportCreating       = String "creating"
    toJSON ReportReady          = String "ready"
instance FromJSON ReportStatus where
    parseJSON (String "pending")    = return ReportPending
    parseJSON (String "creating")   = return ReportCreating
    parseJSON (String "ready")      = return ReportReady
    parseJSON _ = mzero

--
-- ReportType

data ReportType
    = FillsReport
    | AccountReport
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData   ReportType
instance Hashable ReportType

instance ToJSON ReportType where
    toJSON FillsReport                = String "fills"
    toJSON AccountReport              = String "account"
instance FromJSON ReportType where
    parseJSON (String "fills")   = return FillsReport
    parseJSON (String "account") = return AccountReport
    parseJSON _ = mzero

--
-- SelfTrade

data SelfTrade
    = DecreaseAndCancel
    | CancelOldest
    | CancelNewest
    | CancelBoth
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData SelfTrade
instance Hashable SelfTrade
instance ToJSON SelfTrade where
    toJSON DecreaseAndCancel  = String "dc"
    toJSON CancelOldest       = String "co"
    toJSON CancelNewest       = String "cn"
    toJSON CancelBoth         = String "cb"
instance FromJSON SelfTrade where
    parseJSON (String "dc") = return DecreaseAndCancel
    parseJSON (String "co") = return CancelOldest
    parseJSON (String "cn") = return CancelNewest
    parseJSON (String "cb") = return CancelBoth
    parseJSON _ = mzero

--
-- TransferId

newtype TransferId = TransferId { unTransferId :: UUID }
    deriving ( Eq, Ord, Show, Read, Data, Typeable
             , Generic, NFData, FromJSON, ToJSON)

--
-- TransferToCoinbase

data TransferToCoinbase
    = Deposit
        { trAmount            :: Quantity
        , trCoinbaseAccountId :: CoinbaseAccountId
        }
    | Withdraw
        { trAmount            :: Quantity
        , trCoinbaseAccountId :: CoinbaseAccountId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData TransferToCoinbase
instance ToJSON TransferToCoinbase where
    toJSON = genericToJSON coinbaseAesonOptions

--
-- TransferToCoinbaseResponse

data TransferToCoinbaseResponse
    = TransferResponse
        { trId :: TransferId
        -- FIX ME! and other stuff I'm going to ignore.
        } deriving (Eq, Show, Generic, Typeable)

instance NFData   TransferToCoinbaseResponse
instance FromJSON TransferToCoinbaseResponse where
  parseJSON = withObject "TransferToCoinbaseResponse" $ \m ->
    TransferResponse <$> m .: "id"
