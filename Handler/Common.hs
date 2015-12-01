--------------------------------------------------------------------------------
module Handler.Common where

--------------------------------------------------------------------------------
import qualified Data.IntMap as I
import qualified Data.Map.Strict as M
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
publishedPosts :: Handler [PublishedPost]
publishedPosts  = do
    app <- getYesod
    xs  <- liftIO $ fmap (I.elems . _pubs) $ readTVarIO $ _var $ appRep app
    return $ reverse $ sortBy go xs
  where
    go l r = compare (_postDate l) (_postDate r)

--------------------------------------------------------------------------------
publishedPost :: Text -> Handler (Maybe PublishedPost)
publishedPost lnk = do
    app <- getYesod
    liftIO $ atomically $ do
        rep <- readTVar (_var $ appRep app)
        return $ I.lookup (hash lnk) $ _pubs rep

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
