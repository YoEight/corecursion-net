module Handler.Home where

--------------------------------------------------------------------------------
import Import
import Handler.Common
import Repository

--------------------------------------------------------------------------------
getHomeR :: Handler Html
getHomeR = do
    tag_m <- lookupGetParam "tag"
    posts <- case tag_m of
        Nothing  -> publishedPosts
        Just tag -> publishedPostsByTag tag

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
