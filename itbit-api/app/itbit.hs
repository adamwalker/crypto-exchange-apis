{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Maybe
import Control.Monad.Trans.Except

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as BSS
import Data.Aeson
import Options.Applicative
import Control.Error.Util
import Data.Yaml (decodeEither', ParseException)
import System.Directory
import Data.Either.Combinators
import System.Environment
import Network.HTTP.Req hiding (header)

import ItBit

data PartialAPIKey = PartialAPIKey {
    userId    :: Last ByteString,
    publicKey :: Last ByteString,
    secretKey :: Last ByteString
}

instance FromJSON PartialAPIKey where
    parseJSON = withObject "ItBit" $ \k -> do
        v <- (k .:? "ItBit")
        case v of 
            Nothing -> mempty
            Just v -> 
                flip (withObject "APIKey") v $ \v -> PartialAPIKey 
                    <$> (fmap (Last . fmap pack) $ v .:? "UserId")
                    <*> (fmap (Last . fmap pack) $ v .:? "PublicKey")
                    <*> (fmap (Last . fmap pack) $ v .:? "PrivateKey")

instance Monoid PartialAPIKey where
    mempty        = PartialAPIKey mempty mempty mempty
    mappend c1 c2 = PartialAPIKey (Main.userId c1 <> Main.userId c2) (Main.publicKey c1 <> Main.publicKey c2) (Main.secretKey c1 <> Main.secretKey c2)

parseAPIKeys :: Parser PartialAPIKey
parseAPIKeys 
    =   PartialAPIKey
    <$> (Last <$> optional (strOption (long "user-id"     <> metavar "KEY" <> help "User ID")))
    <*> (Last <$> optional (strOption (long "public-key"  <> metavar "KEY" <> help "Public key")))
    <*> (Last <$> optional (strOption (long "private-key" <> metavar "KEY" <> help "Private key")))

apiKeyFromMaybe :: PartialAPIKey -> Either String APIKey
apiKeyFromMaybe PartialAPIKey{..} 
    =   APIKey  
    <$> note "Missing field: UserId"     (getLast userId) 
    <*> note "Missing field: PublicKey"  (getLast publicKey) 
    <*> note "Missing field: PrivateKey" (getLast secretKey)

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
    <$> (hackEnv <$> lookupEnv "ITBIT_PUBLIC_KEY")
    <*> (hackEnv <$> lookupEnv "ITBIT_PUBLIC_KEY")
    <*> (hackEnv <$> lookupEnv "ITBIT_PRIVATE_KEY")
    where
    hackEnv = Last . fmap pack

parseSide :: ReadM Side
parseSide = eitherReader $ \arg -> case arg of
    "Buy"  -> return Buy
    "Sell" -> return Sell
    _      -> Left $ "Cannot parse side"

parseOrder :: Parser Order
parseOrder 
    =   Order
    <$> argument parseSide       (metavar "SIDE")
    <*> pure XBT
    <*> argument auto            (metavar "AMOUNT")
    <*> argument auto            (metavar "PRICE")
    <*> pure XBTUSD

data Command
    = OrderCmd   ByteString Order
    | BalanceCmd 

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [
        command "Order"   (info (OrderCmd <$> (argument auto (metavar "WALLET_ID")) <*> parseOrder) (progDesc "Create an order")),
        command "Balance" (info (pure BalanceCmd)                                                   (progDesc "Get balances"))
    ]

doIt :: (PartialAPIKey, Int, Command) -> ExceptT String IO ()
doIt (cmdlineAPIKey, nonce, command) = do
    fileAPIKey <- getConfigFile
    envAPIKey  <- liftIO getConfigEnv
    apiKey     <- ExceptT $ return $ apiKeyFromMaybe $ fromMaybe mempty fileAPIKey <> envAPIKey <> cmdlineAPIKey

    res <- case command of
        OrderCmd   walletID order -> liftIO $ doRequestPost apiKey nonce (newOrderPath <> walletID <> "/orders") (toJSON order)
        BalanceCmd                -> liftIO $ doRequest     apiKey nonce 

    liftIO $ print $ responseBody res

main :: IO ()
main = execParser opts >>= runExceptT . doIt >>= printErr
    where
    opts = info (helper <*> ((,,) <$> parseAPIKeys <*> parseNonce <*> parseCommand)) (fullDesc <> header "ItBit API")

    parseNonce = argument auto (metavar "NONCE")

    printErr (Left err) = putStrLn err
    printErr (Right _)  = return ()

