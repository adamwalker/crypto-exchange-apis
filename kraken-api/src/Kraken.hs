{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances, FlexibleContexts #-}
module Kraken where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Default.Class
import Data.Maybe
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import Text.Printf

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson
import Network.HTTP.Req
import Data.Digest.Pure.SHA

sign :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
sign secretKey url nonce payload = result
    where
    toHash :: ByteString
    toHash = nonce <> payload

    hashed :: ByteString
    hashed = bytestringDigest $ sha256 toHash

    prepended :: ByteString
    prepended = url <> hashed

    hmaced :: ByteString
    hmaced = bytestringDigest $ hmacSha512 secretKey prepended

    result :: ByteString
    result = B64.encode hmaced

apiRoot :: ByteString
apiRoot =  "https://api.kraken.com"

balancePath :: ByteString
balancePath =  "/0/private/Balance"

addOrderPath :: ByteString
addOrderPath =  "/0/private/AddOrder"

data APIResult a = APIResult {
    error  :: [Value],
    result :: Maybe a
} deriving (Show)

instance FromJSON a => FromJSON (APIResult a) where
    parseJSON = withObject "APIResult" $ \v -> APIResult
        <$> v .:  "error"
        <*> v .:? "result"

data Balances = Balances {
    bch  :: Maybe Double,
    xxbt :: Maybe Double
} deriving (Show)

instance FromJSON Balances where 
    parseJSON = withObject "Balances" $ \v -> Balances
        <$> (fmap read <$> (v .:? "BCH"))
        <*> (fmap read <$> (v .:? "XXBT"))

data AssetPair 
    = XBTUSD

instance ToJSON AssetPair where
    toJSON XBTUSD = "XBTUSD"

data OrderSide  
    = Buy
    | Sell

instance ToJSON OrderSide where
    toJSON Buy  = "buy"
    toJSON Sell = "sell"

data OrderType
    = Market

instance ToJSON OrderType where
    toJSON Market = "market"

data Order = Order {
    assetPair   :: AssetPair,
    orderSide   :: OrderSide,
    orderType   :: OrderType,
    orderVolume :: Double
}

data Nonced a = Nonced {
    nonce :: Int,
    body  :: a
}

instance ToJSON (Nonced Order) where
    toJSON (Nonced nonce Order{..}) = object [
            "nonce"     .= nonce,
            "pair"      .= assetPair,
            "type"      .= orderSide,
            "ordertype" .= orderType,
            "volume"    .= (printf "%.3f" orderVolume :: String)
        ]
    toEncoding (Nonced nonce Order{..}) = pairs $ 
           "nonce"     .= nonce
        <> "pair"      .= assetPair
        <> "type"      .= orderSide
        <> "ordertype" .= orderType
        <> "volume"    .= (printf "%.3f" orderVolume :: String)

instance ToJSON (Nonced ()) where
    toJSON (Nonced nonce ()) = object [
            "nonce"     .= nonce
        ]

data APIKey = APIKey {
    publicKey :: ByteString,
    secretKey :: ByteString
}

instance FromJSON APIKey where
    parseJSON = withObject "APIKey" $ \v -> APIKey
        <$> fmap pack (v .: "PublicKey")
        <*> fmap pack (v .: "PrivateKey")

doRequest :: ToJSON (Nonced a) => APIKey -> ByteString -> a -> IO (JsonResponse Value)
doRequest APIKey{..} path val = runReq def $ do

    currentTime <- liftIO getPOSIXTime
    let nonce = floor $ currentTime * 100

    let payload = Nonced nonce val

    let sig = sign 
            (B64.decodeLenient secretKey)
            path
            (pack $ show nonce)
            (encode payload)

    r <- req 
        POST
        (fst $ fromJust $ parseUrlHttps $ BS.toStrict $ apiRoot <> path)
        (ReqBodyJson payload) 
        jsonResponse
        (
               header "API-Key"  (BS.toStrict publicKey)
            <> header "API-Sign" (BS.toStrict sig)
        )

    return r

    --liftIO $ BS.putStrLn $ encodePretty (responseBody r :: Value)

    --let parsed :: Result (APIResult Balances)
    --    parsed = fromJSON $ responseBody r

    --liftIO $ print parsed

