{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Aggregate.Authors
    ( Authors
    , Author
    , AuthorId
    , buildAuthors
    , lookupAuthor
    , lookupAuthorId
    ) where

--------------------------------------------------------------------------------
import Prelude
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import GHC.Generics

--------------------------------------------------------------------------------
import           ClassyPrelude.Yesod (PathPiece(..))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import           Data.Hashable
import           Data.Text
import           Database.EventStore

------------------------------------------------------------------------------
import Store

------------------------------------------------------------------------------
newtype AuthorId = AuthorId Text deriving (Eq, Ord, Hashable)

-----------------------------------------------------------------------------
newtype Email = Email { emailText :: Text } deriving (Show, FromJSON, ToJSON)

-----------------------------------------------------------------------------
newtype Author = Author { authorEmail :: Email } deriving Show

--------------------------------------------------------------------------------
data AuthorsEvent
    = AuthorCreated !Text !Text
    | GoogleUserAdded !Email
    deriving Generic

-----------------------------------------------------------------------------
instance ToJSON AuthorsEvent where
    toJSON (AuthorCreated l p) =
        object [ "login"    .= l
               , "password" .= p
               ]
    toJSON (GoogleUserAdded e) =
        object [ "action" .= ("new.user" :: Text)
               , "type"   .= ("google" ::Text)
               , "email"  .= e
               ]

    toEncoding (AuthorCreated l p) = pairs ("login" .= l <> "password" .= p)
    toEncoding (GoogleUserAdded e) =
        pairs ( "action" .= ("new.user" :: Text)
              <> "type"  .= ("google" :: Text)
              <> "email" .= e
              )

--------------------------------------------------------------------------------
instance FromJSON AuthorsEvent where
    parseJSON (Object m) = parseOldAuthorCreated
                           <|> parseGoogleUser
                           <|> invalidParse
      where
        parseOldAuthorCreated =
            AuthorCreated <$> (m .: "login") <*> (m .: "password")
        parseGoogleUser = do
            "new.user" :: Text <- m .: "action"
            "google"   :: Text <- m .: "type"
            e                  <- m .: "email"
            return $ GoogleUserAdded e
        invalidParse =
            typeMismatch "AuthorEvent: Invalid Object parse" (Object m)
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
    users m (AuthorCreated _ _) _ = return m
    users m (GoogleUserAdded e) _ =
        let Email l = e in return $ H.insert (AuthorId l) (Author e) m

--------------------------------------------------------------------------------
lookupAuthor :: Authors -> Text -> IO (Maybe Author)
lookupAuthor Authors{..} login = atomically $ do
    m <- readTVar _var
    return $ H.lookup (AuthorId login) m

--------------------------------------------------------------------------------
lookupAuthorId :: Authors -> Text -> IO (Maybe AuthorId)
lookupAuthorId as login =
    fmap (fmap (AuthorId . emailText . authorEmail)) $ lookupAuthor as login
