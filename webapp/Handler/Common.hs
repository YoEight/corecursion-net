--------------------------------------------------------------------------------
module Handler.Common where

--------------------------------------------------------------------------------
import Data.Foldable (for_)

--------------------------------------------------------------------------------
import Data.FileEmbed (embedFile)
import Data.Time

--------------------------------------------------------------------------------
import Aggregate.Post
import Aggregate.Posts
import Import

--------------------------------------------------------------------------------
getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

--------------------------------------------------------------------------------
getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

--------------------------------------------------------------------------------
executePostCommands :: PostId -> [PostCommand] -> Handler ()
executePostCommands postid cmds = do
    app <- getYesod
    liftIO $ do
        pm <- lookupPost (appPosts app) postid
        for_ pm $ \p -> do
            applyCommands p cmds
            refreshHtmlIfPublished (appPosts app) p

-----------------------------------------------------------------------------
getPost :: PostId -> Handler (Maybe Post)
getPost postid = do
    app <- getYesod
    liftIO $ lookupPost (appPosts app) postid

-----------------------------------------------------------------------------
isPublishedF :: Handler (PostId -> Bool)
isPublishedF = do
    app <- getYesod
    liftIO $ publishPredicate (appPosts app)

-----------------------------------------------------------------------------
getPosts :: Handler [(PostId, Post)]
getPosts = do
    app <- getYesod
    liftIO $ posts (appPosts app)

------------------------------------------------------------------------------
publishedPosts :: Handler [PublishedPost]
publishedPosts  = do
    app <- getYesod
    liftIO $ visiblePosts (appPosts app)

--------------------------------------------------------------------------------
lookupPublishedPost :: Text -> Handler (Maybe PublishedPost)
lookupPublishedPost lnk = do
    app <- getYesod
    liftIO $ lookupPublishedPostByLnk (appPosts app) lnk

------------------------------------------------------------------------------
publishedPostsByTag :: Text -> Handler [PublishedPost]
publishedPostsByTag tag = do
    app <- getYesod
    liftIO $ lookupPublishedPostsByTag (appPosts app) tag

--------------------------------------------------------------------------------
aboutPageText :: Handler Text
aboutPageText = do
    app <- getYesod
    liftIO $ fmap aboutText $ about (appPosts app)

--------------------------------------------------------------------------------
aboutPage :: Handler Html
aboutPage = do
    app <- getYesod
    liftIO $ fmap aboutHtml $ about (appPosts app)

--------------------------------------------------------------------------------
addMeta :: Text -> Text -> WidgetT App IO ()
addMeta key value =
    toWidgetHead [hamlet| <meta name=#{key} content=#{value}> |]

--------------------------------------------------------------------------------
parseDate :: Text -> Maybe UTCTime
parseDate txt = parseTimeM True defaultTimeLocale "%Y-%m-%d" str
  where
    str = unpack txt

--------------------------------------------------------------------------------
setAboutContent :: Text -> Handler ()
setAboutContent c = do
    app <- getYesod
    liftIO $ setAbout (appPosts app) c
