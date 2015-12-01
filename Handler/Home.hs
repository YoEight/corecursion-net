module Handler.Home where

--------------------------------------------------------------------------------
import Import
import Handler.Common
import Repository

--------------------------------------------------------------------------------
getHomeR :: Handler Html
getHomeR = do
    posts <- publishedPosts
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
