{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards #-}
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

import Kraken

data PartialAPIKey = PartialAPIKey {
    publicKey :: Last ByteString,
    secretKey :: Last ByteString
}

instance FromJSON PartialAPIKey where
    parseJSON = withObject "Kraken" $ \k -> do
        v <- (k .:? "Kraken")
        case v of 
            Nothing -> mempty
            Just v -> 
                flip (withObject "APIKey") v $ \v -> PartialAPIKey 
                    <$> (fmap (Last . fmap pack) $ v .:? "PublicKey")
                    <*> (fmap (Last . fmap pack) $ v .:? "PrivateKey")

instance Monoid PartialAPIKey where
    mempty        = PartialAPIKey mempty mempty
    mappend c1 c2 = PartialAPIKey (Main.publicKey c1 <> Main.publicKey c2) (Main.secretKey c1 <> Main.secretKey c2)

parseAPIKeys :: Parser PartialAPIKey
parseAPIKeys 
    =   PartialAPIKey
    <$> (Last <$> optional (strOption (long "public-key"  <> metavar "KEY" <> help "Public key")))
    <*> (Last <$> optional (strOption (long "private-key" <> metavar "KEY" <> help "Private key")))

apiKeyFromMaybe :: PartialAPIKey -> Either String APIKey
apiKeyFromMaybe PartialAPIKey{..} 
    =   APIKey  
    <$> note "Missing field: PublicKey"  (getLast publicKey) 
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
    <$> (hackEnv <$> lookupEnv "KRAKEN_PUBLIC_KEY")
    <*> (hackEnv <$> lookupEnv "KRAKEN_PRIVATE_KEY")
    where
    hackEnv = Last . fmap pack

parseSide :: ReadM OrderSide
parseSide = eitherReader $ \arg -> case arg of
    "Buy"  -> return Buy
    "Sell" -> return Sell
    _      -> Left $ "Cannot parse side"

parseOrder :: Parser Order
parseOrder 
    =   Order
    <$> pure XBTUSD
    <*> argument parseSide (metavar "SIDE")
    <*> pure Market
    <*> argument auto      (metavar "AMOUNT")

data Command
    = BalanceCmd
    | OrderCmd Order

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [
        command "Balance" (info (pure BalanceCmd)         (progDesc "Get balances")),
        command "Order"   (info (OrderCmd <$> parseOrder) (progDesc "Create order"))
    ]

doIt :: (PartialAPIKey, Command) -> ExceptT String IO ()
doIt (cmdlineAPIKey, command) = do
    fileAPIKey <- getConfigFile
    envAPIKey  <- liftIO getConfigEnv
    apiKey     <- ExceptT $ return $ apiKeyFromMaybe $ fromMaybe mempty fileAPIKey <> envAPIKey <> cmdlineAPIKey
    
    res <- case command of
        BalanceCmd   -> liftIO $ doRequest apiKey balancePath  ()
        OrderCmd   o -> liftIO $ doRequest apiKey addOrderPath o

    liftIO $ print $ responseBody res

main :: IO ()
main = execParser opts >>= runExceptT . doIt >>= printErr
    where
    opts = info (helper <*> ((,) <$> parseAPIKeys <*> parseCommand)) (fullDesc <> header "Kraken API")

    printErr (Left err) = putStrLn err
    printErr (Right _)  = return ()

