{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Aggregate.Posts where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable (for_)
import Data.List (find, sortBy)
import Data.Maybe
import GHC.Generics
import Prelude

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as I
import qualified Data.SetMap as S
import           Data.Text hiding (find, foldl, reverse)
import           Data.Time
import           Database.EventStore
import           Store

--------------------------------------------------------------------------------
import Aggregate.Post
import Render.Html

-----------------------------------------------------------------------------
-- | A 'Post' that has been tagged published. Meaning its content, in
--   Markdown format has been rendered in HTML.
data PublishedPost =
    PublishedPost
    { _postDate  :: !UTCTime
    , _post      :: !Post
    , _postLink  :: !Text -- permanent link.
    }

--------------------------------------------------------------------------------
data About =
    About
    { aboutText :: !Text
    , aboutHtml :: !Html
    }

--------------------------------------------------------------------------------
data Posts =
    Posts
    { _conn   :: Connection
    , _posts  :: TVar (H.HashMap PostId Post)
    , _pubs   :: TVar (I.IntMap PublishedPost)
    , _tagged :: TVar (S.SetMap Text Int)
    , _about  :: TVar About
    }

--------------------------------------------------------------------------------
data Content =
    Content
    { _cPosts     :: !(H.HashMap PostId Post)
    , _cPubs      :: !(I.IntMap PublishedPost)
    , _cTagged    :: !(S.SetMap Text Int)
    , _cAboutText :: !Text
    , _cAbout     :: !Html
    }

--------------------------------------------------------------------------------
emptyContent :: Content
emptyContent = Content H.empty I.empty S.empty "" mempty

--------------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- API
-----------------------------------------------------------------------------
buildPosts :: Connection -> IO Posts
buildPosts conn = do
    c     <- streamFold conn "posts" content emptyContent
    psVar <- newTVarIO $ _cPosts c
    ppVar <- newTVarIO $ _cPubs c
    aVar  <- newTVarIO $ About (_cAboutText c) (_cAbout c)
    tVar  <- newTVarIO $ _cTagged c

    return Posts
           { _conn   = conn
           , _posts  = psVar
           , _pubs   = ppVar
           , _about  = aVar
           , _tagged = tVar
           }
  where
    content s (PostCreated postid) _ = do
        p <- buildPost conn postid
        return s { _cPosts = H.insert postid p $ _cPosts s }
    content s (PostPublished postid) (Just date) = do
       action <- forM (H.lookup postid $ _cPosts s) $ \p -> do
           sp <- snapshot p
           let tags  = postTags sp
               lnk   = permanentLink sp
               ppost = PublishedPost date p (permanentLink sp)
               pid   = hash lnk
               ts    = foldl (\m t -> S.insert t pid m) (_cTagged s) tags
               s'    = s { _cPubs   = I.insert pid ppost $ _cPubs s
                         , _cTagged = ts
                         }
           return s'
       return $ fromMaybe s action
    content s (PostDeleted postid) _ =
        return s { _cPosts = H.delete postid $ _cPosts s }
    content s (PostUnpublished postid) _ = do
        res <- forM (H.lookup postid $ _cPosts s) $ \p -> do
            sp <- snapshot p
            let lnk = permanentLink sp
                pid = hash lnk
            return s { _cPubs = I.delete pid $ _cPubs s }
        return $ fromMaybe s res
    content s (AboutUpdated c) _ =
        return s { _cAbout     = markdownToHtml c
                 , _cAboutText = c
                 }
    content s _ _ = return s

-----------------------------------------------------------------------------
createPost :: Posts -> Text -> Text -> Text -> [Text] -> IO ()
createPost Posts{..} title content summary tags = do
    let cs = [ SetPostTitle title
             , SetPostContent content
             , SetPostSummary summary
             , SetPostTags tags
             ]

    p <- newPost _conn cs
    let create_evt = PostCreated $ postId p
        saved_evt  = createEvent "post-created" Nothing $ withJson create_evt

    writeNewPost <- sendEvent _conn "posts" anyVersion saved_evt
    atomically $ do
        _ <- waitSTM writeNewPost
        modifyTVar' _posts (H.insert (postId p) p)

------------------------------------------------------------------------------
publishPost :: Posts -> Post -> Maybe UTCTime -> IO ()
publishPost Posts{..} p forcedDate = do
    curDate <- getCurrentTime
    sp <- snapshot p
    let date = fromMaybe curDate forcedDate
        tags      = postTags sp
        pub       = PublishedPost date p (permanentLink sp)
        pub_evt   = PostPublished (postId p)
        pid       = hash $ _postLink pub
        saved_evt = createEvent "post-published" Nothing $
                    withJsonAndMetadata pub_evt date

    writeEvent <- sendEvent _conn "posts" anyVersion saved_evt
    atomically $ do
        _ <- waitSTM writeEvent
        modifyTVar' _pubs (I.insert pid pub)
        for_ tags $ \t -> modifyTVar' _tagged (S.insert t pid)

------------------------------------------------------------------------------
deletePost :: Posts -> Post -> IO ()
deletePost Posts{..} p = do
    let delete_evt = PostDeleted (postId p)
        saved_evt  = createEvent "post-deleted" Nothing $ withJson delete_evt
    writeEvent <- sendEvent _conn "posts" anyVersion saved_evt
    atomically $ do
        _ <- waitSTM writeEvent
        modifyTVar' _posts (H.delete $ postId p)

--------------------------------------------------------------------------------
unpublishPost :: Posts -> Post -> IO ()
unpublishPost Posts{..} p = do
    let unpub_evt = PostUnpublished (postId p)
        saved_evt = createEvent "post-unpublished" Nothing $ withJson unpub_evt
    writeEvent <- sendEvent _conn "posts" anyVersion saved_evt
    atomically $ do
        _  <- waitSTM writeEvent
        sp <- snapshotSTM p
        let lnk = permanentLink sp
            pid = hash lnk
        modifyTVar' _pubs (I.delete pid)

-----------------------------------------------------------------------------
showDate :: UTCTime -> String
showDate d = formatTime defaultTimeLocale "%d %b %Y" d

-----------------------------------------------------------------------------
lookupPost :: Posts -> PostId -> IO (Maybe Post)
lookupPost Posts{..} postid = atomically $ do
    m <- readTVar _posts
    return $ H.lookup postid m

-----------------------------------------------------------------------------
publishPredicate :: Posts -> IO (PostId -> Bool)
publishPredicate Posts{..} = atomically $ do
    m <- readTVar _pubs
    return $ \postid ->
        isJust $ find ((postid ==) . (postId. _post)) m

------------------------------------------------------------------------------
comparePubs :: PublishedPost -> PublishedPost -> Ordering
comparePubs l r = compare (_postDate l) (_postDate r)

-----------------------------------------------------------------------------
posts :: Posts -> IO [(PostId, Post)]
posts Posts{..} = fmap H.toList $ readTVarIO _posts

-----------------------------------------------------------------------------
visiblePosts :: Posts -> IO [PublishedPost]
visiblePosts Posts{..} = atomically $ do
    m <- readTVar _pubs
    return $ reverse $ sortBy comparePubs $ I.elems m

-----------------------------------------------------------------------------
lookupPublishedPostByLnk :: Posts -> Text -> IO (Maybe PublishedPost)
lookupPublishedPostByLnk Posts{..} lnk = atomically $ do
    let pid = hash lnk
    m <- readTVar _pubs
    return $ I.lookup pid m

-----------------------------------------------------------------------------
lookupPublishedPostsByTag :: Posts -> Text -> IO [PublishedPost]
lookupPublishedPostsByTag Posts{..} tag = atomically $ do
    ts <- readTVar _tagged
    ps <- readTVar _pubs
    let set = S.lookup tag ts
        tmp = foldMap (\i -> maybeToList $ I.lookup i ps) set
    return $ reverse $ sortBy comparePubs tmp

-----------------------------------------------------------------------------
about :: Posts -> IO About
about Posts{..} = readTVarIO _about

-----------------------------------------------------------------------------
setAbout :: Posts -> Text -> IO ()
setAbout Posts{..} c = atomically $ do
    a <- readTVar _about
    let new_html = markdownToHtml c
    writeTVar _about a { aboutText = c
                       , aboutHtml = new_html
                       }

-----------------------------------------------------------------------------
publishedPostSnapshot :: PublishedPost -> IO PostSnapshot
publishedPostSnapshot = snapshot . _post

-----------------------------------------------------------------------------
renderPublishedPost :: PublishedPost -> IO Html
renderPublishedPost = renderPost . _post

-----------------------------------------------------------------------------
publishedPostId :: PublishedPost -> PostId
publishedPostId = postId . _post
