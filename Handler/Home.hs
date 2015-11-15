module Handler.Home where

--------------------------------------------------------------------------------
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                               withSmallInput)

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

-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField (withSmallInput "What's on the file?") Nothing
