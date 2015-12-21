{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Aggregate.Authors
    ( Authors
    , Author
    , AuthorId
    , buildAuthors
    , lookupAuthor
    , lookupAuthorId
    , verifyPassword
    ) where

--------------------------------------------------------------------------------
import Prelude
import Control.Concurrent.STM
import Data.Monoid ((<>))
import GHC.Generics

--------------------------------------------------------------------------------
import           ClassyPrelude.Yesod (PathPiece(..))
import qualified Crypto.PasswordStore as Crypto
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
import           Database.EventStore

------------------------------------------------------------------------------
import Store

------------------------------------------------------------------------------
newtype AuthorId = AuthorId Text deriving (Eq, Ord, Hashable)

-----------------------------------------------------------------------------
data Author =
    Author
    { _login :: !Text
    , _passw :: !Text
    } deriving Show

--------------------------------------------------------------------------------
data AuthorsEvent = AuthorCreated !Text !Text deriving Generic

-----------------------------------------------------------------------------
instance ToJSON AuthorsEvent where
    toEncoding (AuthorCreated l p) = pairs ("login" .= l <> "password" .= p)

--------------------------------------------------------------------------------
instance FromJSON AuthorsEvent where
    parseJSON (Object m) =
        AuthorCreated <$> (m .: "login") <*> (m .: "password")
    parseJSON v = typeMismatch "Invalid AuthorEvent" v

------------------------------------------------------------------------------
instance PathPiece AuthorId where
    fromPathPiece = return . AuthorId

    toPathPiece (AuthorId login) = login

--------------------------------------------------------------------------------
data Authors =
    Authors
    { _conn :: Connection
    , _var  :: TVar (H.HashMap AuthorId Author)
    }

--------------------------------------------------------------------------------
buildAuthors :: Connection -> IO Authors
buildAuthors _conn = do
    m    <- streamFold _conn "authors" users H.empty
    _var <- newTVarIO m
    return Authors{..}
  where
    users m (AuthorCreated l p) _ =
        return $ H.insert (AuthorId l) (Author l p) m

--------------------------------------------------------------------------------
lookupAuthor :: Authors -> Text -> IO (Maybe Author)
lookupAuthor Authors{..} login = atomically $ do
    m <- readTVar _var
    return $ H.lookup (AuthorId login) m

--------------------------------------------------------------------------------
lookupAuthorId :: Authors -> Text -> IO (Maybe AuthorId)
lookupAuthorId as login =
    fmap (fmap (AuthorId . _login)) $ lookupAuthor as login

--------------------------------------------------------------------------------
verifyPassword :: Authors -> Author -> Text -> Bool
verifyPassword _ (Author _ cyphered) clear =
    Crypto.verifyPassword (encodeUtf8 clear) (encodeUtf8 cyphered)
