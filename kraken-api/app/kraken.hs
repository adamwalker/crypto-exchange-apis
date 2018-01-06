{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards #-}
module Main where

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

doIt :: (PartialAPIKey, Int) -> ExceptT String IO ()
doIt (cmdlineAPIKey, nonce) = do
    fileAPIKey <- getConfigFile
    envAPIKey  <- liftIO getConfigEnv
    apiKey     <- ExceptT $ return $ apiKeyFromMaybe $ fromMaybe mempty fileAPIKey <> envAPIKey <> cmdlineAPIKey
    void $ liftIO $ doRequest apiKey nonce

main :: IO ()
main = execParser opts >>= runExceptT . doIt >>= printErr
    where
    opts = info (helper <*> ((,) <$> parseAPIKeys <*> parseNonce)) (fullDesc <> header "Kraken API")

    parseNonce = argument auto (metavar "NONCE")

    printErr (Left err) = putStrLn err
    printErr (Right _)  = return ()

