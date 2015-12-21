--------------------------------------------------------------------------------
module Handler.Post where

--------------------------------------------------------------------------------
import Aggregate.Post
import Aggregate.Posts
import Handler.Common
import Import
import Render.Html

--------------------------------------------------------------------------------
getPostR :: Text -> Handler Html
getPostR pid = do
    pm <- lookupPublishedPost pid
    case pm of
        Nothing -> notFound
        Just p  -> defaultLayout $ do
            sp        <- liftIO $ publishedPostSnapshot p
            post_html <- liftIO $ renderPublishedPost p
            toWidget $ CssBuilder $ fromString $ syntaxHighlightingCss
            toWidget $ CssBuilder "code{white-space: pre;}"

            addMeta "description" (postSummary sp)

            setTitle $ toHtml $ postTitle sp
            $(widgetFile "post")
