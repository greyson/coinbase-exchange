{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Rest
    ( makeRequest
    , module Coinbase.Exchange.Types
    ) where

import           Coinbase.Exchange.Internal hiding ( Request )
import qualified Coinbase.Exchange.Internal as I
import           Coinbase.Exchange.Types
import           Crypto.Hash
import           Data.Aeson
import           Data.Byteable                (toBytes)
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as CBS
import qualified Data.ByteString.Lazy         as LBS
import           Data.List                    (intercalate)
import           Data.Time                    (UTCTime)
import           Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Types           as HTTP

import           Debug.Trace

type IsForExchange = Bool

-- | Build an `Network.HTTP.Client.Request` from a Coinbase request
--
-- Since this is entirely pure, it can be combined into Conduit,
-- Pipes, IO, or any other HTTP stacks that you please.
makeRequest :: (ToJSON body)
            => ExchangeConf -> I.Request body response
            -> UTCTime -> HTTP.Request
makeRequest conf r timestamp
  | reqSigning r = signMessage conf False params req timestamp
  | otherwise    = req
  where
    req = encodeBody (reqBody r) emptyReq
    emptyReq = (HTTP.parseRequest_ url)
      { HTTP.method = HTTP.renderStdMethod (reqMethod r)
      , HTTP.requestHeaders = [ ("user-agent", "haskell-simple")
                              , ("accept", "application/json") ]
      }
    url = concat [ endpoint, reqPath r, params ]
    endpoint = endpointRest $ apiType conf
    params
      | null (reqParams r) = ""
      | otherwise = intercalate "&"
        $ map (\(k,v) -> k ++ "=" ++ deUrlEncode v)
        $ reqParams r

encodeBody :: (ToJSON a) => Maybe a -> HTTP.Request -> HTTP.Request
encodeBody Nothing  r = r
encodeBody (Just a) r  = r
    { HTTP.requestHeaders = HTTP.requestHeaders r ++
      [ ("content-type", "application/json") ]
    , HTTP.requestBody = HTTP.RequestBodyBS $ LBS.toStrict $ encode a
    }

signMessage conf isForExchange p req timestamp =
  case authToken conf of
    Nothing -> error "Coinbase Authentication information required."
    Just tok ->
      let
        time = CBS.pack $ show $ round $ utcTimeToPOSIXSeconds timestamp
        presign = CBS.concat [ time, HTTP.method req, CBS.pack p
                             , pullBody (HTTP.requestBody req) ]
        signature = if isForExchange
          then Base64.encode $ toBytes $
               (hmac (secret tok) presign :: HMAC SHA256)
          else digestToHexByteString $ hmacGetDigest
               (hmac (Base64.encode $ secret tok) presign :: HMAC SHA256)
      in
         req{ HTTP.requestHeaders = HTTP.requestHeaders req ++
              [ ("cb-access-key", key tok)
              , ("cb-access-sign", signature)
              , ("cb-access-timestamp", time)
              , if isForExchange
                then ("cb-access-passphrase", passphrase tok)
                else ("cb-version", "2016-05-11")
              ]
            }

  where pullBody (HTTP.RequestBodyBS  b) = b
        pullBody (HTTP.RequestBodyLBS b) = LBS.toStrict b
        pullBody _                  =
          error "Need a bytestring body for authentication"
