{-# LANGUAGE OverloadedStrings, RecordWildCards, RankNTypes, DuplicateRecordFields, ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Maybe
import Control.Monad.Trans.Except
import Control.Monad

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BSS
import Data.Aeson
import Options.Applicative
import Control.Error.Util
import Data.Yaml (decodeEither', ParseException)
import System.Directory
import Data.Either.Combinators
import System.Environment

import BTCMarketsTypes
import BTCMarkets

data PartialAPIKey = PartialAPIKey {
    apiKey        :: Last ByteString,
    apiPrivateKey :: Last ByteString
}

instance FromJSON PartialAPIKey where
    parseJSON = withObject "BTCMarkets" $ \k -> do
        v <- (k .:? "BTCMarkets")
        case v of 
            Nothing -> mempty
            Just v -> 
                flip (withObject "APIKey") v $ \v -> PartialAPIKey 
                    <$> (fmap (Last . fmap pack) $ v .:? "PublicKey")
                    <*> (fmap (Last . fmap pack) $ v .:? "PrivateKey")

instance Monoid PartialAPIKey where
    mempty        = PartialAPIKey mempty mempty
    mappend c1 c2 = PartialAPIKey (Main.apiKey c1 <> Main.apiKey c2) (Main.apiPrivateKey c1 <> Main.apiPrivateKey c2)

parseAPIKeys :: Parser PartialAPIKey
parseAPIKeys = 
        PartialAPIKey
    <$> (Last <$> optional (strOption (long "public-key"  <> metavar "KEY" <> help "Public key")))
    <*> (Last <$> optional (strOption (long "private-key" <> metavar "KEY" <> help "Private key")))

apiKeyFromMaybe :: PartialAPIKey -> Either String APIKey
apiKeyFromMaybe PartialAPIKey{..} 
    =   APIKey  
    <$> note "Missing field: PublicKey"  (getLast apiKey) 
    <*> note "Missing field: PrivateKey" (getLast apiPrivateKey)

getConfigFile :: ExceptT String IO (Maybe PartialAPIKey)
getConfigFile = do
    exists <- liftIO $ doesFileExist "bitcoin.yaml"
    case exists of 
        False -> return Nothing
        True  -> do
            dat <- liftIO $ BSS.readFile "bitcoin.yaml"
            ExceptT $ return $ mapRight Just $ mapLeft show (decodeEither' dat :: Either ParseException PartialAPIKey)

getConfigEnv :: IO PartialAPIKey
getConfigEnv 
    =   PartialAPIKey
    <$> (hackEnv <$> lookupEnv "BTCMARKETS_PUBLIC_KEY")
    <*> (hackEnv <$> lookupEnv "BTCMARKETS_PRIVATE_KEY")
    where
    hackEnv = Last . fmap pack

parseInstrument :: ReadM Instrument
parseInstrument = eitherReader $ \arg -> case arg of
    "BTC" -> return Bitcoin
    "BCH" -> return BitcoinCash
    _     -> Left $ "Cannot parse instrument"

parseOrderHistory :: Parser OrderHistory
parseOrderHistory
    =   OrderHistory
    <$> argument parseInstrument (metavar "INSTRUMENT")
    <*> argument auto (metavar "LIMIT")
    <*> argument auto (metavar "SINCE")

parseTradeHistory :: Parser TradeHistory
parseTradeHistory
    =   TradeHistory
    <$> argument parseInstrument (metavar "INSTRUMENT")
    <*> argument auto (metavar "LIMIT")
    <*> argument auto (metavar "SINCE")

parseOrderDetail :: Parser OrderDetail
parseOrderDetail 
    =   OrderDetail
    <$> (pure <$> argument auto (metavar "ORDERIDS"))

parseSide :: ReadM Side
parseSide = eitherReader $ \arg -> case arg of
    "BID" -> return Bid
    "ASK" -> return Ask
    _     -> Left $ "Cannot parse side"

parseOrderType :: ReadM OrderType
parseOrderType = eitherReader $ \arg -> case arg of
    "LIMIT"  -> return Limit
    "MARKET" -> return Market
    _        -> Left $ "Cannot parse order type"

parseCreateOrder :: Parser CreateOrder
parseCreateOrder 
    =   CreateOrder
    <$> argument parseInstrument (metavar "INSTRUMENT")
    <*> argument auto (metavar "PRICE")
    <*> ((floor . (* 10 ** 5)) <$> argument (auto :: ReadM Double) (metavar "VOLUME"))
    <*> argument parseSide (metavar "SIDE")
    <*> argument parseOrderType (metavar "TYPE")

parseCancelOrder :: Parser CancelOrder
parseCancelOrder 
    =   CancelOrder
    <$> (pure <$> argument auto (metavar "ORDERIDS"))

data Command
    = OrderHistoryCmd OrderHistory
    | TradeHistoryCmd TradeHistory
    | OrderDetailCmd  OrderDetail
    | BalanceCmd
    | TradingFeeCmd
    | CreateOrderCmd  CreateOrder
    | CancelOrderCmd  CancelOrder
    deriving (Show)

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [
        command "OrderHistory" (info (OrderHistoryCmd     <$> parseOrderHistory) (progDesc "Order history")),
        command "TradeHistory" (info (TradeHistoryCmd     <$> parseTradeHistory) (progDesc "Trade history")),
        command "OrderDetail"  (info (OrderDetailCmd      <$> parseOrderDetail)  (progDesc "Order detail")),
        command "Balance"      (info (pure BalanceCmd)                           (progDesc "Get balance")),
        command "TradingFee"   (info (pure TradingFeeCmd)                        (progDesc "Get trading fee")),
        command "CreateOrder"  (info (CreateOrderCmd      <$> parseCreateOrder)  (progDesc "Create order")),
        command "CancelOrder"  (info (CancelOrderCmd      <$> parseCancelOrder)  (progDesc "Cancel order"))
    ]

doIt :: (PartialAPIKey, Command) -> ExceptT String IO ()
doIt (cmdlineAPIKey, command) = do

    fileAPIKey <- getConfigFile
    envAPIKey  <- liftIO getConfigEnv
    apiKey     <- ExceptT $ return $ apiKeyFromMaybe $ fromMaybe mempty fileAPIKey <> envAPIKey <> cmdlineAPIKey

    case command of
        OrderHistoryCmd orderHistory -> void $ liftIO $ sendPost apiKey "/order/history"              orderHistory
        TradeHistoryCmd tradeHistory -> void $ liftIO $ sendPost apiKey "/order/trade/history"        tradeHistory
        OrderDetailCmd  orderDetail  -> void $ liftIO $ sendPost apiKey "/order/detail"               orderDetail
        BalanceCmd                   -> do
            res <- liftIO $ sendGet  apiKey "/account/balance"
            liftIO $ print $ (fromJSON res :: Result [BalanceResponse])
        TradingFeeCmd                -> void $ liftIO $ sendGet  apiKey "/account/BTC/AUD/tradingfee"
        CancelOrderCmd cancelOrder   -> void $ liftIO $ sendPost apiKey "/order/cancel"               cancelOrder
        CreateOrderCmd createOrder   -> void $ liftIO $ sendPost apiKey "/order/create"               createOrder

main = execParser opts >>= runExceptT . doIt >>= printErr
    where 
    opts = info (helper <*> ((,) <$> parseAPIKeys <*> parseCommand)) (fullDesc <> header "bitcoin")

    printErr (Left err) = putStrLn err
    printErr (Right _)  = return ()

