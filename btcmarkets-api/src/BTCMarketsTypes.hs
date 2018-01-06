{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards, LambdaCase #-}
module BTCMarketsTypes where

import Data.Monoid

import Data.Aeson

data Instrument 
    = Bitcoin
    | BitcoinCash
    deriving (Show)

instance ToJSON Instrument where    
    toJSON Bitcoin     = String "BTC"
    toJSON BitcoinCash = String "BCH"

data OrderHistory = OrderHistory {
    instrument :: Instrument,
    limit      :: Int,
    since      :: Int
} deriving (Show)

instance ToJSON OrderHistory where
    toJSON OrderHistory{..} = object [
        "currency"   .= ("AUD" :: String),
        "instrument" .= instrument,
        "limit"      .= limit,
        "since"      .= since
        ]
    toEncoding OrderHistory{..} = pairs $
           "currency"   .= ("AUD" :: String)
        <> "instrument" .= instrument
        <> "limit"      .= limit
        <> "since"      .= since

data TradeHistory = TradeHistory {
    instrument :: Instrument,
    limit      :: Int,
    since      :: Int
} deriving (Show)

instance ToJSON TradeHistory where
    toJSON TradeHistory{..} = object [
        "currency"   .= ("AUD" :: String),
        "instrument" .= instrument,
        "limit"      .= limit,
        "since"      .= since
        ]
    toEncoding TradeHistory{..} = pairs $
           "currency"   .= ("AUD" :: String)
        <> "instrument" .= instrument
        <> "limit"      .= limit
        <> "since"      .= since

data OrderDetail = OrderDetail {
    orderIDs :: [Int]
} deriving (Show)

instance ToJSON OrderDetail where
    toJSON OrderDetail{..} = object [
            "orderIds" .= orderIDs
        ]

data Side
    = Bid
    | Ask
    deriving (Show)

instance ToJSON Side where
    toJSON Bid = String "Bid"
    toJSON Ask = String "Ask"

data OrderType
    = Limit
    | Market
    deriving (Show)

instance ToJSON OrderType where
    toJSON Limit  = "Limit"
    toJSON Market = "Market"

data CreateOrder = CreateOrder {
    instrument :: Instrument,
    price      :: Int,
    volume     :: Int,
    side       :: Side,
    orderType  :: OrderType
} deriving (Show)

instance ToJSON CreateOrder where
    toJSON CreateOrder{..} = object [
            "currency"        .= ("AUD" :: String),
            "instrument"      .= instrument,
            "price"           .= price,
            "volume"          .= volume,
            "orderSide"       .= side,
            "ordertype"       .= orderType,
            "clientRequestId" .= ("13245" :: String)
        ]
    toEncoding CreateOrder{..} = pairs $ 
               "currency"        .= ("AUD" :: String)
            <> "instrument"      .= instrument
            <> "price"           .= price
            <> "volume"          .= volume
            <> "orderSide"       .= side
            <> "ordertype"       .= orderType
            <> "clientRequestId" .= ("13245" :: String)

data CancelOrder = CancelOrder {
    orderIDs :: [Int]
} deriving (Show)

instance ToJSON CancelOrder where
    toJSON CancelOrder{..} = object [
            "orderIds" .= orderIDs
        ]

--Responses

data Currency 
    = BCH
    | AUD 
    | BTC
    | LTC
    | ETH
    | XRP
    | ETC
    | USD
    deriving (Show, Eq)

instance FromJSON Currency where
    parseJSON = withText "Currency" $ \case
        "BCH" -> pure BCH
        "AUD" -> pure AUD
        "BTC" -> pure BTC
        "LTC" -> pure LTC
        "ETH" -> pure ETH
        "XRP" -> pure XRP
        "ETC" -> pure ETC
        "USD" -> pure USD
        _     -> fail "Currency"

data BalanceResponse = BalanceResponse {
    pendingFunds :: Int,
    balance      :: Int,
    currency     :: Currency
} deriving (Show)

instance FromJSON BalanceResponse where 
    parseJSON = withObject "BalanceResponse" $ \v -> BalanceResponse
        <$> v .: "pendingFunds" 
        <*> v .: "balance"
        <*> v .: "currency"

