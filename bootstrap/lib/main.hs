{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import Data.Int
import GHC.Generics

--------------------------------------------------------------------------------
import Database.EventStore
import qualified Data.Attoparsec.Text as Atto
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
data RunCmd =
    RunCmd
    { _storeIp         :: !String
    , _storePort       :: !Int
    , _storeAdminLogin :: !(Maybe B.ByteString)
    , _storeAdminPassw :: !(Maybe B.ByteString)
    , _cmd             :: !Cmd
    }

--------------------------------------------------------------------------------
data Cmd
    = CreateUserCmd CreateUser
    | SetMaxAgeCmd SetMaxAge

--------------------------------------------------------------------------------
parseCmd :: Parser RunCmd
parseCmd = RunCmd
           <$> parseIp
           <*> parsePort
           <*> parseAdminLogin
           <*> parseAdminPassword
           <*> subparser (createUserCmd <> setMaxAgeCmd)

--------------------------------------------------------------------------------
newtype CreateUser = CreateUser { _username :: T.Text }

--------------------------------------------------------------------------------
createUserCmd :: Mod CommandFields Cmd
createUserCmd = command "useradd" (info parser desc)
  where
    parser = fmap (CreateUserCmd . CreateUser) parseUserLogin
    desc   = progDesc "Add a new author user."

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
data TimePart
    = Days Int64
    | Hours Int64
    | Mins Int64
    | Secs Int64
    deriving Show

--------------------------------------------------------------------------------
data SetMaxAge =
    SetMaxAge
    { _setMaxStream :: !T.Text
    , _setMaxPart   :: !TimePart
    }

--------------------------------------------------------------------------------
setMaxAgeCmd :: Mod CommandFields Cmd
setMaxAgeCmd = command "maxage" (info parser desc)
  where
    parser =
        fmap SetMaxAgeCmd $
            SetMaxAge
            <$> parseSetMaxStream
            <*> parseSetMaxPart
    desc = progDesc "Set stream $maxAge property"

--------------------------------------------------------------------------------
parseSetMaxStream :: Parser T.Text
parseSetMaxStream = fmap T.pack $ strOption m
  where
    m =    short 's'
        <> long "stream"
        <> metavar "STREAM"
        <> help "An event stream."

--------------------------------------------------------------------------------
parseSetMaxPart :: Parser TimePart
parseSetMaxPart = option (eitherReader go) m
  where
    m =    long "time"
        <> metavar "TIME"
        <> help "Maximum time a stream should keep its events."

    go i = Atto.parseOnly parseTimePart (T.pack i)

--------------------------------------------------------------------------------
parsePart :: T.Text -> Atto.Parser Int64
parsePart p = do
    _ <- Atto.string p
    _ <- Atto.char '='
    Atto.decimal

--------------------------------------------------------------------------------
parseDays :: Atto.Parser TimePart
parseDays = fmap Days $ parsePart "days"

--------------------------------------------------------------------------------
parseHours :: Atto.Parser TimePart
parseHours = fmap Hours $ parsePart "hours"

--------------------------------------------------------------------------------
parseMins :: Atto.Parser TimePart
parseMins = fmap Mins $ parsePart "mins"

--------------------------------------------------------------------------------
parseSecs :: Atto.Parser TimePart
parseSecs = fmap Secs $ parsePart "secs"

--------------------------------------------------------------------------------
parseTimePart :: Atto.Parser TimePart
parseTimePart = parseDays <|> parseHours <|> parseMins

--------------------------------------------------------------------------------
executeCmd :: RunCmd -> IO ()
executeCmd RunCmd{..} = do
    let creds = credentials <$> _storeAdminLogin <*> _storeAdminPassw
        setts = defaultSettings { s_credentials = creds }

    conn <- connect setts _storeIp _storePort
    case _cmd of
        CreateUserCmd CreateUser{..} -> do
            let e   = GoogleUserAdded _username
                evt = createEvent "user-created" Nothing $ withJson e
            act <- sendEvent conn "authors" anyVersion evt
            _   <- wait act
            return ()
        SetMaxAgeCmd SetMaxAge{..} -> do
            let span =
                    case _setMaxPart of
                        Days i  -> timeSpanFromDays $ realToFrac i
                        Hours i -> timeSpanFromHours $ realToFrac i
                        Mins i  -> timeSpanFromMinutes $ realToFrac i
                        Secs i  -> timeSpanFromSeconds $ realToFrac i

                meta = buildStreamMetadata (setMaxAge span)
            _ <- setStreamMetadata conn _setMaxStream anyVersion meta >>= wait
            return ()

--------------------------------------------------------------------------------
main :: IO ()
main = do
    let i = info (helper <*> parseCmd)
            ( fullDesc
              <> progDesc "Initialize corecursion.net database."
              <> header "bootstrap - Operates on corecursion.net database."
            )

    execParser i >>= executeCmd
