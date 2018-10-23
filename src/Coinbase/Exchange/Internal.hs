module Coinbase.Exchange.Internal where

import Data.Aeson
import Data.Aeson.Types
import Data.List
  ( intercalate )
import Data.Scientific
import Data.Text
  ( Text, pack, unpack )
import Data.Text.Encoding
  ( decodeUtf8 )
import Network.HTTP.Types
  ( StdMethod )

import qualified Data.ByteString as BS

data Request a response = Request
  { reqEndpoint :: ApiType -> String
  , reqMethod   :: StdMethod
  , reqSigning  :: Bool
  , reqPath     :: String
  , reqParams   :: [(String, UrlEncoded)]
  , reqBody     :: Maybe a
  }

request :: (ApiType -> String) -> StdMethod -> Bool -> String -> Maybe body
        -> Request body response
request e m s p Nothing  = Request e m s p [] Nothing
request e m s p (Just j) = Request e m s p [] (Just j)

-- * Endpoint determination

data ApiType
    = Sandbox
    | Live
    deriving (Show)

endpointRest Sandbox = "https://api-public.sandbox.pro.coinbase.com"
endpointRest Live    = "https://api.pro.coinbase.com"

endpointSocket Sandbox = "ws-feed-public.sandbox.pro.coinbase.com"
endpointSocket Live    = "ws-feed.pro.coinbase.com"

-- Coinbase needs to provide real BTC transfers through the exchange
-- API soon, making 2 API calls with 2 sets of authentication
-- credentials is ridiculous.

endpointRealRest Sandbox = "https://api.sandbox.coinbase.com"
endpointRealRest Live    = "https://api.coinbase.com"

-- * Quick and dirty URL encoding

-- (URL representation should be separate from Show representation
-- since types may already have instances of the latter which are
-- unsuited to the necessary URL encoded representation.

class UrlEncode a where
  urlParam :: a -> String

instance UrlEncode Scientific where
  urlParam = formatScientific Fixed (Just 8)
instance UrlEncode Text where
  urlParam = unpack
instance UrlEncode Int where
  urlParam = show
instance UrlEncode Integer where
  urlParam = show
instance UrlEncode BS.ByteString where
  urlParam = urlParam . decodeUtf8

newtype UrlEncoded = UrlEncoded { deUrlEncode :: String }

-- WARNING: Does not escape, should be unnecessary
urlEncode :: UrlEncode a => a -> UrlEncoded
urlEncode = UrlEncoded . urlParam

urlParams :: [(String, UrlEncoded)] -> String
urlParams [] = ""
urlParams ps = "?" ++ (intercalate "&" $
   map (\(k, v) -> k ++ "=" ++ deUrlEncode v) ps)

(.&) :: (UrlEncode v) => Request a r -> (String, v) -> Request a r
req .& (n, v) =
  let enc = [(n, UrlEncoded (urlParam v))]
  in req{ reqParams = reqParams req ++ enc }

(.&?) :: (UrlEncode v) => Request a r -> (String, Maybe v) -> Request a r
req .&? (_, Nothing) = req
req .&? (n, Just v)  = req .& (n, v)
