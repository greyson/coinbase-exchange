{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Types.Core where

import           Coinbase.Exchange.Internal
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import           Data.Hashable
import           Data.Int
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Scientific
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           Data.Tuple          (swap)
import           Data.UUID
import           Data.Void
import           Data.Word
import           GHC.Generics
import           Text.Read           (readMaybe)

--
-- Aggregate

newtype Aggregate = Aggregate { unAggregate :: Int64 }
    deriving (Eq, Ord, Show, Read, Num, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

--
-- ClientOrderId

newtype ClientOrderId = ClientOrderId { unClientOrderId :: UUID }
    deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
             , NFData, Hashable, FromJSON, ToJSON)

--
-- Coin scientific (8-decimal fixed scientific parser from string)

newtype CoinScientific = CoinScientific { unCoinScientific :: Scientific }
    deriving ( Eq, Ord, Num, Fractional, Real, RealFrac, Show
             , Read, Data, Typeable, NFData, Hashable)

instance ToJSON CoinScientific where
    toJSON (CoinScientific v) =
      String . T.pack . formatScientific Fixed (Just 8) $ v
instance FromJSON CoinScientific where
    parseJSON = withText "CoinScientific" $ \t ->
        case readMaybe (T.unpack t) of
            Just  n -> pure $ CoinScientific n
            Nothing -> fail "Could not parse string scientific."

--
-- Cost

newtype Cost = Cost { unCost :: CoinScientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON)
instance ToJSON Cost where
    toJSON (Cost (CoinScientific v)) = String . T.pack . formatScientific Fixed Nothing $ v
instance Show Cost where
    show (Cost (CoinScientific v)) = formatScientific Fixed Nothing v

--
-- CurrencyId

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
             , IsString, NFData, Hashable, FromJSON, ToJSON )

--
-- Empty

data Empty = Empty
instance FromJSON Empty where
  parseJSON = withObject "Empty" $ \_ -> return Empty

--
-- OrderId

newtype OrderId = OrderId { unOrderId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)
instance UrlEncode OrderId where
  urlParam = toString . unOrderId

--
-- OrderStatus

data OrderStatus
    = OrderDone
    | Settled
    | Open
    | Pending
    | Active
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

osList = [ (OrderDone, "done"),  (Settled, "settled"), (Open, "open")
         , (Pending, "pending"), (Active, "active")  ]
osOut = M.fromList osList
osIn  = M.fromList $ swap <$> osList

instance NFData OrderStatus
instance Hashable OrderStatus
instance ToJSON OrderStatus where
    toJSON = String . (osOut M.!)
instance FromJSON OrderStatus where
    parseJSON = withText "OrderStatus" $ \s ->
      maybe (fail $ "Unknown order status: " ++ T.unpack s) return
      $ M.lookup s osIn
instance UrlEncode OrderStatus where
  urlParam = map toLower . show

--
-- OrderType

data OrderType = Limit | Market
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData OrderType
instance Hashable OrderType
instance ToJSON OrderType where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON OrderType where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- Price

newtype Price = Price { unPrice :: Scientific }
  deriving ( Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read
           , Data, Typeable, Generic, NFData, Hashable )
instance ToJSON Price where
  toJSON v =
    String . T.pack . formatScientific Fixed Nothing . unPrice $ v
instance FromJSON Price where
  parseJSON = withText "Price" $ \t ->
    maybe (fail $ "Could not parse price: " ++ T.unpack t)
          (pure . Price)
          $ readMaybe (T.unpack t)

--
-- ProductId

newtype ProductId = ProductId { unProductId :: Text }
    deriving ( Eq, Ord, Show, Read, Data, Typeable
             , Generic, IsString, NFData, Hashable, FromJSON, ToJSON)

instance UrlEncode ProductId where urlParam = T.unpack . unProductId

--
-- Quantity

newtype Quantity = Quantity { unQuantity :: Scientific }
  deriving ( Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read
           , Data, Typeable, Generic, NFData, Hashable )
instance ToJSON Quantity where
  toJSON v =
    String . T.pack . formatScientific Fixed Nothing . unQuantity $ v
instance FromJSON Quantity where
  parseJSON = withText "Price" $ \t ->
    maybe (fail $ "Could not parse price: " ++ T.unpack t)
          (pure . Quantity)
          $ readMaybe (T.unpack t)

--
-- Reason

data Reason = Filled | Canceled
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData Reason
instance Hashable Reason
instance ToJSON Reason where
    toJSON = genericToJSON defaultOptions {
      constructorTagModifier = map toLower }
instance FromJSON Reason where
    parseJSON = genericParseJSON defaultOptions {
      constructorTagModifier = map toLower }

--
-- Sequence

newtype Sequence = Sequence { unSequence :: Word64 }
    deriving ( Eq, Ord, Num, Enum, Show, Read, Data, Typeable
             , Generic, NFData, Hashable, FromJSON, ToJSON)

--
-- Side

data Side = Buy | Sell
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData Side
instance Hashable Side
instance ToJSON Side where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Side where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
-- Size

newtype Size = Size { unSize :: CoinScientific }
    deriving ( Eq, Ord, Num, Fractional, Real, RealFrac
             , Read, Data, Typeable, Generic, NFData, Hashable, FromJSON)
instance ToJSON Size where
    toJSON (Size s) = toJSON s
instance Show Size where
    show (Size s) = show s

--
-- TradeId

newtype TradeId = TradeId { unTradeId :: Word64 }
    deriving (Eq, Ord, Num, Show, Read, Data, Typeable, Generic, NFData, Hashable)

instance ToJSON TradeId where
    toJSON = String . T.pack . show . unTradeId
instance FromJSON TradeId where
    parseJSON (String t) = pure $ TradeId $ read $ T.unpack t
    parseJSON (Number n) = pure $ TradeId $ floor n
    parseJSON _          = mzero
