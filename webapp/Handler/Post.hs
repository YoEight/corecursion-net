--------------------------------------------------------------------------------
module Handler.Post where

--------------------------------------------------------------------------------
import Handler.Common
import Import
import Repository

--------------------------------------------------------------------------------
getPostR :: Text -> Handler Html
getPostR pid = do
    pm <- publishedPost pid
    case pm of
        Nothing -> notFound
        Just p  -> defaultLayout $ do
            toWidget $ CssBuilder $ fromString $ syntaxHighlightingCss
            toWidget $ CssBuilder "code{white-space: pre;}"

            addMeta "description" (_postSummary $ _post p)

            setTitle $ toHtml $ _postTitle $ _post p
            $(widgetFile "post")
