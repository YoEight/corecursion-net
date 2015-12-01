{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Repository where

-----------------------------------------------------------------------------
import Prelude
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

-----------------------------------------------------------------------------
import           Database.EventStore
import           Data.Aeson
import           Data.Hashable
import qualified Data.IntMap as I
import qualified Data.Map.Strict as M
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import           Data.Time
import           Data.UUID (UUID, toString)
import           Data.UUID.V4
import           Text.Blaze.Html
import           Text.Highlighting.Kate (haddock, styleToCss)
import           Text.Pandoc

-----------------------------------------------------------------------------
import Model
import Store

-----------------------------------------------------------------------------
data Post =
    Post
    { _postTitle   :: !Text
    , _postContent :: !Text
    , _postSummary :: !Text
    , _postTags    :: ![Text]
    } deriving Show

-----------------------------------------------------------------------------
emptyPost :: Post
emptyPost = Post "" "" "" []

-----------------------------------------------------------------------------
-- | A 'Post' that has been tagged published. Meaning its content, in
--   Markdown format has been rendered in HTML.
data PublishedPost =
    PublishedPost
    { _postDate  :: !UTCTime
    , _postHtml  :: !Html
    , _post      :: !Post
    , _postLink  :: !Text -- permanent link.
    }

-----------------------------------------------------------------------------
showDate :: PublishedPost -> String
showDate p = formatTime defaultTimeLocale "%d %b %Y" pdate
  where
    pdate = _postDate p

-----------------------------------------------------------------------------
data User =
    User
    { _usrLogin :: !Text
    , _usrPassw :: !Text
    } deriving Show

-----------------------------------------------------------------------------
type Posts = M.Map PostId Post

-----------------------------------------------------------------------------
type PublishedPosts = I.IntMap PublishedPost

-----------------------------------------------------------------------------
type Users = M.Map UserId User

-----------------------------------------------------------------------------
-- | Holds unpublished and published posts in one place.
data Internal =
    Internal
    { _posts     :: !Posts
    , _pubs      :: !PublishedPosts
    , _usrs      :: !Users
    , _about     :: !Html
    , _aboutText :: !Text
    }

-----------------------------------------------------------------------------
emptyInternal :: Internal
emptyInternal = Internal M.empty I.empty M.empty mempty mempty

-----------------------------------------------------------------------------
-- | Exposed as the entry point to access or update posts.
data Repository =
    Repository
    { _conn :: !Connection
    , _var  :: !(TVar Internal)
    }

------------------------------------------------------------------------------
postStream :: PostId -> Text
postStream (PostId uuid) = "post:" <> (pack $ toString uuid)

--------------------------------------------------------------------------------
readerOpts :: ReaderOptions
readerOpts = def { readerExtensions = githubMarkdownExtensions }

--------------------------------------------------------------------------------
writerOpts :: WriterOptions
writerOpts = def { writerHighlight      = True
                 , writerHighlightStyle = haddock
                 }

--------------------------------------------------------------------------------
getHtml :: Pandoc -> Html
getHtml = writeHtml writerOpts

--------------------------------------------------------------------------------
readContentMarkdown :: Text -> Pandoc
readContentMarkdown txt =
    case readMarkdown readerOpts $ unpack txt of
        Left e    -> error $ show e
        Right doc -> doc

--------------------------------------------------------------------------------
syntaxHighlightingCss :: String
syntaxHighlightingCss = styleToCss haddock

-----------------------------------------------------------------------------
permanentLink :: Post -> Text
permanentLink = T.map go . _postTitle
  where
    go ' ' = '_'
    go x   = x

-----------------------------------------------------------------------------
newRepository :: Connection -> IO Repository
newRepository conn = do
    repo  <- streamFold conn "posts" content emptyInternal
    repo' <- streamFold conn "authors" users repo
    var   <- newTVarIO repo'
    return $ Repository conn var
  where
    content rep (PostCreated postid) _ = do
        p <- streamFold conn (postStream postid) post emptyPost
        let rep' = rep { _posts = M.insert postid p $ _posts rep }
        return rep'
    content rep (PostPublished postid) (Just date) =
        let action = do
                p <- M.lookup postid $ _posts rep
                let doc   = readContentMarkdown $ _postContent p
                    nodes = writeHtml writerOpts doc
                    lnk   = permanentLink p
                    ppost = PublishedPost date nodes p (permanentLink p)
                    pid   = hash lnk
                    rep'  = rep { _pubs = I.insert pid ppost $ _pubs rep }
                return rep' in
        return $ fromMaybe rep action
    content rep (PostDeleted postid) _ = do
        let rep' = rep { _posts = M.delete postid $ _posts rep }
        return rep'
    content rep (PostUnpublished postid) _ = do
        res <- forM (M.lookup postid $ _posts rep) $ \p -> do
            let lnk  = permanentLink p
                pid  = hash lnk
                rep' = rep { _pubs = I.delete pid $ _pubs rep }
            return rep'
        return $ fromMaybe rep res
    content rep (AboutUpdated c) _ = do
        let doc  = readContentMarkdown c
            html = writeHtml writerOpts doc
            rep' = rep { _about     = html
                       , _aboutText = c
                       }
        return rep'
    content rep _ _ = return rep

    post p (PostContentUpdated c) _ =
        return p { _postContent = c }
    post p (PostTitleUpdated t) _ =
        return p { _postTitle = t }
    post p (PostTagsUpdated ts) _ =
        return p { _postTags = ts }
    post p (PostSummaryUpdated s) _ =
        return p { _postSummary = s }

    users rep (UserCreated l p) _ =
        let usr  = User l p
            rep' = rep { _usrs = M.insert (UserId l) usr $ _usrs rep } in
        return rep'

-----------------------------------------------------------------------------
applyPostCommand :: Post -> PostCommand -> Post
applyPostCommand p (SetPostContent c) = p { _postContent = c }
applyPostCommand p (SetPostTitle t)   = p { _postTitle = t }
applyPostCommand p (SetPostTags t)    = p { _postTags  = t }
applyPostCommand p (SetPostSummary s) = p { _postSummary = s }

-----------------------------------------------------------------------------
postCommandToEvent :: PostCommand -> Event
postCommandToEvent (SetPostContent c) =
    createEvent "post-content-updated" Nothing $
      withJson (toJSON $ PostContentUpdated c)
postCommandToEvent (SetPostTitle t) =
    createEvent "post-title-updated" Nothing $
      withJson (toJSON $ PostTitleUpdated t)
postCommandToEvent (SetPostTags t) =
    createEvent "post-tags-updated" Nothing $
      withJson (toJSON $ PostTagsUpdated t)
postCommandToEvent (SetPostSummary s) =
    createEvent "post-summary-updated" Nothing $
      withJson (toJSON $ PostSummaryUpdated s)

-----------------------------------------------------------------------------
handlePostCommands :: Repository -> UUID -> [PostCommand] -> IO ()
handlePostCommands Repository{..} uuid cmds = do
    res <- atomically $ do
        rep <- readTVar _var
        let pid = PostId uuid
        case M.lookup pid $ _posts rep of
            Nothing -> return Nothing
            Just p  -> do
                let p' = foldl applyPostCommand p cmds
                    m' = M.insert pid p' $ _posts rep
                writeTVar _var rep { _posts = m' }
                return $ Just pid
    forM_ res $ \pid -> do
        let evts = fmap postCommandToEvent cmds
        act <- sendEvents _conn (postStream pid) anyVersion evts
        wait act

-----------------------------------------------------------------------------
handleContentCommand :: Repository -> ContentCommand -> IO ()
handleContentCommand Repository{..} (CreatePost t c s ts) = do
    uuid <- nextRandom
    date <- getCurrentTime
    let postid = PostId uuid
        post   = Post t c s ts
        e      = PostCreated postid
        edata  = withJsonAndMetadata (toJSON e) (toJSON date)
        stream = postStream postid
        evt    = createEvent "post-created" Nothing edata
        batch  = [ createEvent "post-title-updated"
                               Nothing
                               (withJson $ toJSON $ PostTitleUpdated t)
                 , createEvent "post-content-updated"
                               Nothing
                               (withJson $ toJSON $ PostContentUpdated c)
                 , createEvent "post-tags-updated"
                               Nothing
                               (withJson $ toJSON $ PostTagsUpdated ts)
                 , createEvent "post-summary-updated"
                               Nothing
                               (withJson $ toJSON $ PostSummaryUpdated s)
                 ]
    atomically $ do
        rep <- readTVar _var
        let rep' = rep { _posts = M.insert postid post $ _posts rep }
        writeTVar _var rep'
    act1 <- sendEvents _conn stream noStreamVersion batch
    act2 <- sendEvent _conn "posts" anyVersion evt
    _    <- wait act1
    _    <- wait act2
    return ()
handleContentCommand Repository{..} (PublishPost forcedDate postid) = do
    curDate <- getCurrentTime
    let date = fromMaybe curDate forcedDate
    done    <- atomically $ do
        rep <- readTVar _var
        case M.lookup postid $ _posts rep of
            Nothing -> return False
            Just p  -> do
                let doc   = readContentMarkdown $ _postContent p
                    nodes = writeHtml writerOpts doc
                    lnk   = permanentLink p
                    pid   = hash lnk
                    ppost = PublishedPost date nodes p lnk
                    rep'  = rep { _pubs = I.insert pid ppost $ _pubs rep }
                writeTVar _var rep'
                return True
    when done $ do
        let e   = PostPublished postid
            evt = createEvent "post-published" Nothing $ withJsonAndMetadata
                                                         (toJSON e)
                                                         (toJSON date)
        act <- sendEvent _conn "posts" anyVersion evt
        _   <- wait act
        return ()
handleContentCommand Repository{..} (DeletePost postid) = do
    done <- atomically $ do
        rep <- readTVar _var
        case M.lookup postid $ _posts rep of
            Nothing -> return False
            Just _  -> do
                let rep' = rep { _posts = M.delete postid $ _posts rep }
                writeTVar _var rep'
                return True
    when done $ do
        let e   = PostDeleted postid
            evt = createEvent "post-deleted" Nothing $ withJson (toJSON e)
        act <- sendEvent _conn "posts" anyVersion evt
        _   <- wait act
        return ()
handleContentCommand Repository{..} (UnpublishPost postid) = do
    done <- atomically $ do
        rep <- readTVar _var
        case M.lookup postid $ _posts rep of
            Nothing -> return False
            Just p  -> do
                let lnk  = permanentLink p
                    pid  = hash lnk
                    rep' = rep { _pubs = I.delete pid $ _pubs rep }
                writeTVar _var rep'
                return True
    when done $ do
        let e   = PostUnpublished postid
            evt = createEvent "post-unpublished" Nothing $ withJson (toJSON e)
        act <- sendEvent _conn "posts" anyVersion evt
        _   <- wait act
        return ()
handleContentCommand Repository{..} (SetAbout c) = do
    atomically $ do
        rep <- readTVar _var
        let doc  = readContentMarkdown c
            html = writeHtml writerOpts doc
            rep' = rep { _about     = html
                       , _aboutText = c
                       }
        writeTVar _var rep'
    let e   = AboutUpdated c
        evt = createEvent "about-updated" Nothing $ withJson (toJSON e)
    act <- sendEvent _conn "posts" anyVersion evt
    _   <- wait act
    return ()

--------------------------------------------------------------------------------
lookupUserId :: Repository -> Text -> IO (Maybe UserId)
lookupUserId rep login = fmap (fmap (UserId . _usrLogin)) $ lookupUser rep login

--------------------------------------------------------------------------------
lookupUser :: Repository -> Text -> IO (Maybe User)
lookupUser Repository{..} login = atomically $ do
    rep <- readTVar _var
    return $ M.lookup (UserId login) $ _usrs rep

--------------------------------------------------------------------------------
parseDate :: Text -> Maybe UTCTime
parseDate txt = parseTimeM True defaultTimeLocale "%Y-%m-%d" str
  where
    str = unpack txt
