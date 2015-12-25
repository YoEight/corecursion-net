{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Aggregate.Post
    ( PostId
    , Post
    , PostSnapshot(..)
    , PostCommand(..)
    , postIdText
    , buildPost
    , newPost
    , applyCommands
    , snapshot
    , snapshotSTM
    , postId
    , renderPost
    , permanentLink
    ) where

------------------------------------------------------------------------------
import Control.Concurrent.STM
import GHC.Generics
import Prelude

------------------------------------------------------------------------------
import           ClassyPrelude.Yesod (PathPiece(..))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import qualified Data.Text as T
import           Data.UUID.V4
import           Data.UUID
import           Database.EventStore

------------------------------------------------------------------------------
import Render.Html
import Store

------------------------------------------------------------------------------
-- | Post events
data PostEvent
    = PostContentUpdated !T.Text
      -- ^ Indicates a post's content was updated.
    | PostTitleUpdated !T.Text
      -- ^ Indicates a post's title was changed.
    | PostTagsUpdated ![T.Text]
      -- ^ Indicates post's tags were updated.
    | PostSummaryUpdated !T.Text
      -- ^ Indicates post's summary was updated.
    deriving Generic

------------------------------------------------------------------------------
instance FromJSON PostEvent
instance ToJSON PostEvent

------------------------------------------------------------------------------
newtype PostId = PostId UUID deriving (Eq, Ord, Read, Generic)

------------------------------------------------------------------------------
instance Show PostId where
    show (PostId uuid) = show uuid

------------------------------------------------------------------------------
instance ToJSON PostId where
    toJSON (PostId uuid) = String $ T.pack $ toString uuid

------------------------------------------------------------------------------
instance FromJSON PostId where
    parseJSON (String str) =
        case fromString $ T.unpack str of
          Just uuid -> return $ PostId uuid
          _         -> typeMismatch "Invalid PostId UUID" (String str)
    parseJSON v = typeMismatch "Invalid PostId" v

------------------------------------------------------------------------------
instance Hashable PostId where
    hash (PostId uuid) = hash $ toASCIIBytes uuid

------------------------------------------------------------------------------
instance PathPiece PostId where
    fromPathPiece = fmap PostId . fromText

    toPathPiece (PostId t) = toText t

------------------------------------------------------------------------------
postIdText :: PostId -> T.Text
postIdText (PostId uuid) = T.pack $ show uuid

------------------------------------------------------------------------------
data PostSnapshot =
    PostSnapshot
    { postTitle   :: !T.Text
    , postSummary :: !T.Text
    , postContent :: !T.Text
    , postTags    :: ![T.Text]
    }

------------------------------------------------------------------------------
emptyPostSnapshot :: PostSnapshot
emptyPostSnapshot = PostSnapshot "" "" "" []

------------------------------------------------------------------------------
data Post =
    Post
    { _conn :: Connection
    , _id   :: PostId
    , _var  :: TVar PostSnapshot
    }

--------------------------------------------------------------------------------
postStream :: PostId -> T.Text
postStream (PostId uuid) = "post:" <> (T.pack $ toString uuid)

------------------------------------------------------------------------------
buildPost :: Connection -> PostId -> IO Post
buildPost conn postid = do
    s   <- streamFold conn (postStream postid) post emptyPostSnapshot
    var <- newTVarIO s
    return $ Post conn postid var
  where
    post p (PostContentUpdated c) _ =
        return p { postContent = c }
    post p (PostTitleUpdated t) _ =
        return p { postTitle = t }
    post p (PostTagsUpdated ts) _ =
        return p { postTags = ts }
    post p (PostSummaryUpdated s) _ =
        return p { postSummary = s }

------------------------------------------------------------------------------
newPost :: Connection -> [PostCommand] -> IO Post
newPost _conn cs = do
    uuid <- nextRandom

    let evts = fmap postCommandToEvent cs
        _id  = PostId uuid

    writeEvents <- sendEvents _conn (postStream _id) noStreamVersion evts
    atomically $ do
        _ <- waitSTM writeEvents
        let p = foldl applyPostCommand emptyPostSnapshot cs
        _var <- newTVar p
        return Post{..}

--------------------------------------------------------------------------------
-- | Actions supported by a post entity.
data PostCommand
    = SetPostContent !T.Text
      -- ^ Updates post text content.
    | SetPostTitle !T.Text
      -- ^ Updates post title.
    | SetPostTags ![T.Text]
      -- ^ Updates post tags.
    | SetPostSummary !T.Text
      -- ^ Updates post summary

--------------------------------------------------------------------------------
postCommandToEvent :: PostCommand -> Event
postCommandToEvent (SetPostContent c) =
    createEvent "post-content-updated" Nothing $ withJson (PostContentUpdated c)
postCommandToEvent (SetPostTitle t) =
    createEvent "post-title-updated" Nothing $ withJson (PostTitleUpdated t)
postCommandToEvent (SetPostTags t) =
    createEvent "post-tags-updated" Nothing $ withJson (PostTagsUpdated t)
postCommandToEvent (SetPostSummary s) =
    createEvent "post-summary-updated" Nothing $ withJson (PostSummaryUpdated s)

-----------------------------------------------------------------------------
applyPostCommand :: PostSnapshot -> PostCommand -> PostSnapshot
applyPostCommand p (SetPostContent c) = p { postContent = c }
applyPostCommand p (SetPostTitle t)   = p { postTitle = t }
applyPostCommand p (SetPostTags t)    = p { postTags  = t }
applyPostCommand p (SetPostSummary s) = p { postSummary = s }

--------------------------------------------------------------------------------
applyCommands :: Post -> [PostCommand] -> IO ()
applyCommands Post{..} cs = do
    let evts = fmap postCommandToEvent cs

    writeEvents <- sendEvents _conn (postStream _id) anyVersion evts
    atomically $ do
        _ <- waitSTM writeEvents
        modifyTVar' _var (flip (foldl applyPostCommand) cs)

--------------------------------------------------------------------------------
snapshot :: Post -> IO PostSnapshot
snapshot Post{..} = readTVarIO _var

--------------------------------------------------------------------------------
snapshotSTM :: Post -> STM PostSnapshot
snapshotSTM Post{..} = readTVar _var

--------------------------------------------------------------------------------
postId :: Post -> PostId
postId = _id

--------------------------------------------------------------------------------
renderPost :: Post -> IO Html
renderPost p = fmap (markdownToHtml . postContent) $ snapshot p

-----------------------------------------------------------------------------
permanentLink :: PostSnapshot -> T.Text
permanentLink = T.map go . postTitle
  where
    go ' ' = '_'
    go x   = x
