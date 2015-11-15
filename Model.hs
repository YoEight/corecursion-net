{-# LANGUAGE DeriveGeneric #-}
------------------------------------------------------------------------------
module Model where

------------------------------------------------------------------------------
import ClassyPrelude.Yesod hiding (fromString)

------------------------------------------------------------------------------
import Data.UUID

------------------------------------------------------------------------------
newtype UserId = UserId Text deriving (Eq, Ord)

------------------------------------------------------------------------------
instance PathPiece UserId where
    fromPathPiece t = Just $ UserId t
    toPathPiece (UserId t) = t

------------------------------------------------------------------------------
newtype PostId = PostId UUID deriving (Eq, Ord, Read)

------------------------------------------------------------------------------
instance PathPiece PostId where
    fromPathPiece t = do
        uuid <- fromString $ unpack t
        return $ PostId uuid

    toPathPiece (PostId t) = pack $ toString t

------------------------------------------------------------------------------
instance Show PostId where
    show (PostId uuid) = toString uuid

------------------------------------------------------------------------------
instance ToJSON PostId where
    toJSON (PostId uuid) = String $ pack $ toString uuid

------------------------------------------------------------------------------
instance FromJSON PostId where
    parseJSON (String str) =
        case fromString $ unpack str of
          Just uuid -> return $ PostId uuid
          _         -> mzero
    parseJSON _ = mzero

------------------------------------------------------------------------------
postIdText :: PostId -> Text
postIdText = pack . show

------------------------------------------------------------------------------
-- | Actions supported by the post repository.
data ContentCommand
    = CreatePost !Text !Text !Text ![Text]
      -- ^ Creates a new post given a title, a content and a list of tags.
    | PublishPost (Maybe UTCTime) !PostId
      -- ^ Allows the post to be available publicly.
    | DeletePost !PostId
      -- ^ Deletes an unpublished post.
    | UnpublishPost !PostId
      -- ^ Unpublishes a publish post.
    | SetAbout !Text
      -- ^ Set about page content.

------------------------------------------------------------------------------
-- | Actions supported by a post entity.
data PostCommand
    = SetPostContent !Text
      -- ^ Updates post text content.
    | SetPostTitle !Text
      -- ^ Updates post title.
    | SetPostTags ![Text]
      -- ^ Updates post tags.
    | SetPostSummary !Text
      -- ^ Updates post summary

------------------------------------------------------------------------------
-- | Post repository events.
data ContentEvent
    = PostCreated !PostId
      -- ^ Indicates a new post was created
    | PostPublished !PostId
      -- ^ Indicates a post was publised.
    | PostDeleted !PostId
      -- ^ Indicates a post was deleted.
    | PostUnpublished !PostId
      -- ^ Indicates a published post was unpublished.
    | AboutUpdated !Text
      -- ^ Indicates about page was updated.
    deriving Generic

------------------------------------------------------------------------------
instance FromJSON ContentEvent
instance ToJSON ContentEvent

------------------------------------------------------------------------------
-- | Post events
data PostEvent
    = PostContentUpdated !Text
      -- ^ Indicates a post's content was updated.
    | PostTitleUpdated !Text
      -- ^ Indicates a post's title was changed.
    | PostTagsUpdated ![Text]
      -- ^ Indicates post's tags were updated.
    | PostSummaryUpdated !Text
      -- ^ Indicates post's summary was updated.
    deriving Generic

------------------------------------------------------------------------------
instance FromJSON PostEvent
instance ToJSON PostEvent

-----------------------------------------------------------------------------
data UsersEvent = UserCreated !Text !Text

-----------------------------------------------------------------------------
instance ToJSON UsersEvent where
    toJSON (UserCreated l p) =
        object [ "login"    .= l
               , "password" .= p
               ]

--------------------------------------------------------------------------------
instance FromJSON UsersEvent where
    parseJSON (Object m) = UserCreated <$> (m .: "login") <*> (m .: "password")
    parseJSON _          = mzero
