{-# LANGUAGE OverloadedStrings, LambdaCase, DuplicateRecordFields, RecordWildCards #-}
module ItBit where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Default.Class
import Data.Maybe
import Text.Printf

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import Data.Aeson
import Network.HTTP.Req
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA

sign :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
sign secretKey url nonce ts = result
    where
    toHash :: ByteString
    toHash = nonce <> "[\"GET\",\"" <> url <> "\",\"\",\"" <> nonce <> "\",\"" <> ts <> "\"]"

    hashed :: ByteString
    hashed = bytestringDigest $ sha256 toHash

    prepended :: ByteString
    prepended = url <> hashed

    hmaced :: ByteString
    hmaced = bytestringDigest $ hmacSha512 secretKey prepended

    result :: ByteString
    result = B64.encode hmaced

escape :: String -> String
escape = concatMap func 
    where
    func '"' = "\\\""
    func x   = [x]

signPost :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> (ByteString, ByteString)
signPost secretKey url body nonce ts = (toHash, result)
    where
    toHash :: ByteString
    toHash = nonce <> "[\"POST\",\"" <> url <> "\",\"" <> BS.pack (escape (BS.unpack body)) <> "\",\"" <> nonce <> "\",\"" <> ts <> "\"]"

    hashed :: ByteString
    hashed = bytestringDigest $ sha256 toHash

    prepended :: ByteString
    prepended = url <> hashed

    hmaced :: ByteString
    hmaced = bytestringDigest $ hmacSha512 secretKey prepended

    result :: ByteString
    result = B64.encode hmaced

data Currency
    = USD
    | XBT
    | EUR
    | SGD
    deriving (Show, Eq)

instance ToJSON Currency where
    toJSON USD = "USD"
    toJSON XBT = "XBT"
    toJSON EUR = "EUR"
    toJSON SGD = "SGD"

instance FromJSON Currency where
    parseJSON = withText "Currency" $ \case
        "USD" -> pure USD
        "XBT" -> pure XBT
        "EUR" -> pure EUR
        "SGD" -> pure SGD
        _     -> fail "Currency"

data Balance = Balance {
    total     :: Double,
    available :: Double,
    currency  :: Currency 
} deriving (Show)

instance FromJSON Balance where
    parseJSON = withObject "Balance" $ \v -> Balance
        <$> (read <$> (v .: "totalBalance"))
        <*> (read <$> (v .: "availableBalance"))
        <*> v .: "currency"

data Wallet = Wallet {
    balances :: [Balance],
    userId   :: String,
    name     :: String,
    id       :: String
} deriving (Show)

instance FromJSON Wallet where
    parseJSON = withObject "Wallet" $ \v -> Wallet
        <$> v .: "balances"  
        <*> v .: "userId"
        <*> v .: "name"
        <*> v .: "id"

data Instrument 
    = XBTUSD
    deriving (Show)

instance ToJSON Instrument where
    toJSON XBTUSD = String "XBTUSD"

data Side 
    = Buy
    | Sell
    deriving (Show)

instance ToJSON Side where
    toJSON Buy = String "buy"
    toJSON Sell = String "sell"

data Order = Order {
    side       :: Side,
    currency   :: Currency,
    amount     :: Double,
    price      :: Double,
    instrument :: Instrument
} deriving (Show)

instance ToJSON Order where
    toJSON Order{..} = object [
            "side"       .= side,
            "type"       .= ("limit" :: String),
            "currency"   .= currency,
            "amount"     .= (printf "%.4f" amount :: String),
            "display"    .= (printf "%.4f" amount :: String),
            "price"      .= (printf "%.2f" price :: String),
            "instrument" .= instrument
        ]
    toEncoding Order{..} = pairs $
           "side"       .= side
        <> "type"       .= ("limit" :: String)
        <> "currency"   .= currency
        <> "amount"     .= (printf "%.4f" amount :: String)
        <> "display"    .= (printf "%.4f" amount :: String)
        <> "price"      .= (printf "%.2f" price  :: String)
        <> "instrument" .= instrument

apiRoot :: ByteString
apiRoot = "https://api.itbit.com"

walletsPath :: ByteString
walletsPath = "/v1/wallets"

newOrderPath :: ByteString
newOrderPath = "/v1/wallets/"

data APIKey = APIKey {
    userId     :: ByteString,
    publicKey  :: ByteString,
    privateKey :: ByteString
}

instance FromJSON APIKey where
    parseJSON = withObject "APIKey" $ \v -> APIKey
        <$> fmap pack (v .: "UserId")
        <*> fmap pack (v .: "PublicKey")
        <*> fmap pack (v .: "PrivateKey")

doRequest :: APIKey -> Int -> IO (JsonResponse Value)
doRequest APIKey{..} nonce' = runReq def $ do

    currentTime <- liftIO $ getPOSIXTime

    let nonce = pack $ show nonce'

    let signature = sign 
            privateKey 
            (apiRoot <> walletsPath <> "?" <> "userId=" <> userId)
            nonce
            (pack (show (floor (currentTime * 1000))))

    r <- req 
        GET -- method
        (fst $ fromJust $ parseUrlHttps $ BS.toStrict $ apiRoot <> walletsPath)
        NoReqBody
        jsonResponse -- specify how to interpret response
        (
               ("userId" =: BS.unpack userId)
            <> header "Authorization"    (BS.toStrict publicKey <> ":" <> BS.toStrict signature)
            <> header "X-Auth-Timestamp" (BSS.pack (show (floor (currentTime * 1000))))
            <> header "X-Auth-Nonce"     (BS.toStrict nonce)
            <> header "Content-Type"     "application/json"
        )

    return r

    --liftIO $ BS.putStrLn $ encodePretty (responseBody r :: Value)

    --let parsed :: Result [Wallet]
    --    parsed = fromJSON $ responseBody r

    --liftIO $ print parsed

doRequestPost :: APIKey -> Int -> ByteString -> Value -> IO (JsonResponse Value)
doRequestPost APIKey{..} nonce' path payload = runReq def $ do

    currentTime <- liftIO $ getPOSIXTime

    let nonce = pack $ show nonce'

    let (toSign, signature) = signPost
            privateKey 
            (apiRoot <> path)
            (encode payload)
            nonce
            (pack (show (floor (currentTime * 1000))))

    liftIO $ BS.putStrLn toSign

    r <- req 
        POST -- method
        (fst $ fromJust $ parseUrlHttps $ BS.toStrict $ apiRoot <> path)
        (ReqBodyJson payload)
        jsonResponse -- specify how to interpret response
        (
               header "Authorization"    (BS.toStrict publicKey <> ":" <> BS.toStrict signature)
            <> header "X-Auth-Timestamp" (BSS.pack (show (floor (currentTime * 1000))))
            <> header "X-Auth-Nonce"     (BS.toStrict nonce)
            <> header "Content-Type"     "application/json"
        )

    liftIO $ print $ responseBody r

    return r

