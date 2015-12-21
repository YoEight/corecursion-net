module Handler.Home where

--------------------------------------------------------------------------------
import Aggregate.Post
import Aggregate.Posts
import Import
import Handler.Common

--------------------------------------------------------------------------------
getHomeR :: Handler Html
getHomeR = do
    tag_m <- lookupGetParam "tag"
    pubs  <- case tag_m of
        Nothing  -> publishedPosts
        Just tag -> publishedPostsByTag tag

    snapshots <- traverse (liftIO . snapshot . _post) pubs

    defaultLayout $ do
        setTitle "Home"
        $(widgetFile "homepage")

--------------------------------------------------------------------------------
getAboutR :: Handler Html
getAboutR = do
    content <- aboutPage
    defaultLayout $ do
        setTitle "About"
        $(widgetFile "about")
