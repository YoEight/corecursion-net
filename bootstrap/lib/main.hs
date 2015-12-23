{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import GHC.Generics

--------------------------------------------------------------------------------
import Database.EventStore
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Data.Time
import Options.Applicative
import Text.Read

--------------------------------------------------------------------------------
data UsersEvent = GoogleUserAdded !T.Text deriving Generic

--------------------------------------------------------------------------------
data Params =
    Params
    { _storeIp         :: !String
    , _storePort       :: !Int
    , _storeAdminLogin :: !(Maybe B.ByteString)
    , _storeAdminPassw :: !(Maybe B.ByteString)
    , _storeUsername   :: !T.Text
    }

--------------------------------------------------------------------------------
parseParams :: Parser Params
parseParams = Params
              <$> parseIp
              <*> parsePort
              <*> parseAdminLogin
              <*> parseAdminPassword
              <*> parseUserLogin

--------------------------------------------------------------------------------
parseIp :: Parser String
parseIp = strOption m
  where
    m =    long "ip"
        <> metavar "IP"
        <> help "EventStore's IP address."
        <> value "127.0.0.1"
        <> showDefault

--------------------------------------------------------------------------------
parsePort :: Parser Int
parsePort = option (eitherReader go) m
  where
    m =    long "port"
        <> metavar "PORT"
        <> help "EventStore's port."
        <> value 1113
        <> showDefault

    go i =
        case readMaybe i of
            Nothing -> Left "Invalid integer."
            Just p  -> Right p

--------------------------------------------------------------------------------
parseAdminLogin :: Parser (Maybe B.ByteString)
parseAdminLogin = option (eitherReader (Right . Just . B.pack)) m
  where
    m =    short 'l'
        <> long "store-login"
        <> metavar "STORE_LOGIN"
        <> help "Store user login."
        <> value Nothing
        <> showDefaultWith (const "No admin login used.")

--------------------------------------------------------------------------------
parseAdminPassword :: Parser (Maybe B.ByteString)
parseAdminPassword = option (eitherReader (Right . Just . B.pack)) m
  where
    m =    short 'p'
        <> long "store-passw"
        <> metavar "STORE_PASSW"
        <> help "Store user password."
        <> value Nothing
        <> showDefaultWith (const "No admin password used.")

--------------------------------------------------------------------------------
parseUserLogin :: Parser T.Text
parseUserLogin = option (eitherReader (Right . T.pack)) m
  where
    m =    long "username"
        <> metavar "USERNAME"
        <> help "Author's login."

--------------------------------------------------------------------------------
instance ToJSON UsersEvent where
    toJSON (GoogleUserAdded e) =
        object [ "action" .= ("new.user" :: T.Text)
               , "type"   .= ("google" :: T.Text)
               , "email"  .= e
               ]

    toEncoding (GoogleUserAdded e) =
        pairs ( "action" .= ("new.user" :: T.Text)
              <> "type"  .= ("google" :: T.Text)
              <> "email" .= e
              )

--------------------------------------------------------------------------------
main :: IO ()
main = do
    let i = info parseParams
            ( fullDesc
              <> progDesc "Initialize corecursion.net database."
              <> header "bootstrap - add author to corecursion.net."
            )

    Params{..} <- execParser i

    let creds = credentials <$> _storeAdminLogin <*> _storeAdminPassw
        setts = defaultSettings { s_credentials = creds }

    let e   = GoogleUserAdded _storeUsername
        evt = createEvent "user-created" Nothing $ withJson e

    conn <- connect setts _storeIp _storePort
    act  <- sendEvent conn "authors" anyVersion evt
    _    <- wait act
    return ()
