{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import Crypto.PasswordStore
import Database.EventStore
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Data.Time

--------------------------------------------------------------------------------
data UsersEvent = UserCreated !Text !Text

--------------------------------------------------------------------------------
instance ToJSON UsersEvent where
    toJSON (UserCreated l p) =
        object [ "login"    .= l
               , "password" .= p
               ]

--------------------------------------------------------------------------------
default_strength :: Int
default_strength = 17

--------------------------------------------------------------------------------
store_login :: ByteString
store_login = "eventstore"

--------------------------------------------------------------------------------
store_pwd :: ByteString
store_pwd = "password"

--------------------------------------------------------------------------------
store_ip :: String
store_ip = "1.2.3.4"

--------------------------------------------------------------------------------
store_port :: Int
store_port = 1234

--------------------------------------------------------------------------------
admin_login :: Text
admin_login = "user"

--------------------------------------------------------------------------------
admin_pwd :: ByteString
admin_pwd = "password"

--------------------------------------------------------------------------------
main :: IO ()
main = do
    let creds = credentials store_login store_pwd
        setts = defaultSettings
                { s_credentials = Just creds
                , s_retry       = keepRetrying
                }
    passw <- fmap decodeUtf8 $ makePassword admin_pwd default_strength
    let e   = UserCreated admin_login passw
        evt = createEvent "user-created" Nothing $ withJson e

    conn <- connect setts store_ip store_port
    act  <- sendEvent conn "authors" anyVersion evt
    _    <- wait act
    return ()
