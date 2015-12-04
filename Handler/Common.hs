--------------------------------------------------------------------------------
module Handler.Common where

--------------------------------------------------------------------------------
import qualified Data.IntMap as I
import qualified Data.Map.Strict as M
import qualified Data.SetMap as S
import           Data.FileEmbed (embedFile)
import           Data.Hashable
import           Data.UUID

--------------------------------------------------------------------------------
import Import
import Repository

--------------------------------------------------------------------------------
getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

--------------------------------------------------------------------------------
getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

--------------------------------------------------------------------------------
executeContentCommand :: ContentCommand -> Handler ()
executeContentCommand cmd = do
    app <- getYesod
    liftIO $ handleContentCommand (appRep app) cmd

--------------------------------------------------------------------------------
executePostCommands :: PostId -> [PostCommand] -> Handler ()
executePostCommands (PostId uuid) cmds = do
    app <- getYesod
    liftIO $ handlePostCommands (appRep app) uuid cmds

-----------------------------------------------------------------------------
getPost :: PostId -> Handler (Maybe Post)
getPost postid = do
    app <- getYesod
    liftIO $ do
        dat <- readTVarIO $ _var $ appRep app
        return $ M.lookup postid $ _posts dat

-----------------------------------------------------------------------------
getPostId :: UUID -> Handler (Maybe PostId)
getPostId uuid = do
    app <- getYesod
    liftIO $ do
        dat <- readTVarIO $ _var $ appRep app
        let postid = PostId uuid
        if M.member postid $ _posts dat
            then return $ Just postid
            else return Nothing

-----------------------------------------------------------------------------
postPreview :: PostId -> Handler (Maybe PublishedPost)
postPreview postid = do
    app <- getYesod
    liftIO $ do
        date <- getCurrentTime
        dat  <- readTVarIO $ _var $ appRep app
        case M.lookup postid $ _posts dat of
            Nothing -> return Nothing
            Just p  ->
                let doc  = readContentMarkdown $ _postContent p
                    html = getHtml doc in
                return $ Just $ PublishedPost date html p (permanentLink p)

-----------------------------------------------------------------------------
isPublishedF :: Handler (PostId -> Bool)
isPublishedF = do
    app <- getYesod
    liftIO $ do
        rep <- readTVarIO $ _var $ appRep app
        return $ \postid ->
            case M.lookup postid $ _posts rep of
                Nothing -> False
                Just p  ->
                    let lnk = permanentLink p
                        pid = hash lnk in
                    I.member pid $ _pubs rep

-----------------------------------------------------------------------------
getPosts :: Handler [(PostId, Post)]
getPosts = do
    app <- getYesod
    liftIO $ do
        dat <- readTVarIO $ _var $ appRep app
        return $ M.assocs $ _posts dat

------------------------------------------------------------------------------
comparePubs :: PublishedPost -> PublishedPost -> Ordering
comparePubs l r = compare (_postDate l) (_postDate r)

------------------------------------------------------------------------------
publishedPosts :: Handler [PublishedPost]
publishedPosts  = do
    app <- getYesod
    xs  <- liftIO $ fmap (I.elems . _pubs) $ readTVarIO $ _var $ appRep app
    return $ reverse $ sortBy comparePubs xs

--------------------------------------------------------------------------------
publishedPost :: Text -> Handler (Maybe PublishedPost)
publishedPost lnk = do
    app <- getYesod
    liftIO $ atomically $ do
        rep <- readTVar (_var $ appRep app)
        return $ I.lookup (hash lnk) $ _pubs rep

------------------------------------------------------------------------------
publishedPostsByTag :: Text -> Handler [PublishedPost]
publishedPostsByTag tag = do
    app <- getYesod
    atomically $ do
        s   <- readTVar $ _var $ appRep app
        let set = S.lookup tag $ _tagged s
            tmp = foldMap (\i -> maybeToList $ I.lookup i $ _pubs s) set
        return $ reverse $ sortBy comparePubs tmp

--------------------------------------------------------------------------------
aboutPageText :: Handler Text
aboutPageText = do
    app <- getYesod
    liftIO $ atomically $ do
        rep <- readTVar (_var $ appRep app)
        return $ _aboutText rep

--------------------------------------------------------------------------------
aboutPage :: Handler Html
aboutPage = do
    app <- getYesod
    liftIO $ atomically $ do
        rep <- readTVar (_var $ appRep app)
        return $ _about rep
