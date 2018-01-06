{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes, DuplicateRecordFields #-}
module BTCMarkets where

import Control.Monad.IO.Class
import Data.Monoid
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Maybe

import Data.Aeson.Encode.Pretty hiding (Config)
import Data.Aeson
import Data.Default.Class
import Network.HTTP.Req
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA

sign 
    :: ByteString -- ^ Timestamp
    -> ByteString -- ^ API key
    -> ByteString -- ^ URL
    -> ByteString -- ^ Message to sign
    -> ByteString -- ^ Signature
sign ts apiKey url msg = B64.encode $ bytestringDigest $ hmacSha512 apiKey toSign
    where
    toSign :: ByteString
    toSign = url <> "\n" <> ts <> "\n" <> msg

data APIKey = APIKey {
    apiKey        :: ByteString,
    apiPrivateKey :: ByteString
}

sendPost :: ToJSON a => APIKey -> ByteString -> a -> IO Value
sendPost APIKey{..} path payload = runReq def $ do

    currentTime <- liftIO $ getPOSIXTime

    let currentTimeBS :: ByteString 
        currentTimeBS = pack (show (floor (currentTime * 1000)))

    let sig = sign 
            currentTimeBS 
            (B64.decodeLenient apiPrivateKey)
            path
            (encode payload)

    r <- req 
        POST -- method
        (fst $ fromJust $ parseUrlHttps $ "https://api.btcmarkets.net" <> BS.toStrict path)
        (ReqBodyJson payload)
        jsonResponse -- specify how to interpret response
        (
               header "Accept"           "application/json"
            <> header "Accept-Charset"   "UTF-8"
            <> header "Content-Type"     "application/json"
            <> header "apikey"           (BS.toStrict apiKey)
            <> header "timestamp"        (BS.toStrict currentTimeBS)
            <> header "signature"        (BS.toStrict sig)
        )

    liftIO $ BS.putStrLn $ encodePretty (responseBody r :: Value)
    return $ responseBody r

signGet
    :: ByteString -- ^ Timestamp
    -> ByteString -- ^ API key
    -> ByteString -- ^ URL
    -> ByteString -- ^ Signature
signGet ts apiKey url = B64.encode $ bytestringDigest $ hmacSha512 apiKey toSign
    where
    toSign :: ByteString
    toSign = url <> "\n" <> ts <> "\n"

sendGet :: APIKey -> ByteString -> IO Value
sendGet APIKey{..} path = runReq def $ do

    currentTime <- liftIO $ getPOSIXTime

    let currentTimeBS :: ByteString 
        currentTimeBS = pack (show (floor (currentTime * 1000)))

    let sig = signGet
            currentTimeBS 
            (B64.decodeLenient apiPrivateKey)
            path

    r <- req 
        GET -- method
        (fst $ fromJust $ parseUrlHttps $ "https://api.btcmarkets.net" <> BS.toStrict path)
        NoReqBody
        jsonResponse -- specify how to interpret response
        (
               header "Accept"           "application/json"
            <> header "Accept-Charset"   "UTF-8"
            <> header "Content-Type"     "application/json"
            <> header "apikey"           (BS.toStrict apiKey)
            <> header "timestamp"        (BS.toStrict currentTimeBS)
            <> header "signature"        (BS.toStrict sig)
        )

    liftIO $ BS.putStrLn $ encodePretty (responseBody r :: Value)
    return $ responseBody r

