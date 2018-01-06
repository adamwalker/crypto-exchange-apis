{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Kraken where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Default.Class
import Data.Maybe

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

data APIKey = APIKey {
    publicKey :: ByteString,
    secretKey :: ByteString
}

instance FromJSON APIKey where
    parseJSON = withObject "APIKey" $ \v -> APIKey
        <$> fmap pack (v .: "PublicKey")
        <*> fmap pack (v .: "PrivateKey")

doRequest :: APIKey -> Int -> IO (JsonResponse Value)
doRequest APIKey{..} nonce = runReq def $ do

    let payload = object [ 
            "nonce" .= (nonce :: Int)
            ]

    let sig = sign 
            (B64.decodeLenient secretKey)
            balancePath
            (pack $ show nonce)
            (encode payload)

    r <- req 
        POST
        (fst $ fromJust $ parseUrlHttps $ BS.toStrict $ apiRoot <> balancePath)
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

