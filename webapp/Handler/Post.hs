--------------------------------------------------------------------------------
module Handler.Post where

--------------------------------------------------------------------------------
import Network.Wai
import Network.Socket.Internal

--------------------------------------------------------------------------------
import Aggregate.Post
import Aggregate.Posts
import Aggregate.Stats
import Handler.Common
import Import
import Render.Html

--------------------------------------------------------------------------------
getPostR :: Text -> Handler Html
getPostR pid = do
    pm    <- lookupPublishedPost pid
    stats <- getsYesod appStats
    req   <- waiRequest
    case pm of
        Nothing -> notFound
        Just p  -> defaultLayout $ do
            sp        <- liftIO $ publishedPostSnapshot p
            post_html <- liftIO $ renderPublishedPost p
            let postid = publishedPostId p
                ip     = showAddr $ remoteHost req
            liftIO $ postView stats postid ip
            toWidget $ CssBuilder $ fromString $ syntaxHighlightingCss
            toWidget $ CssBuilder "code{white-space: pre;}"

            addMeta "description" (postSummary sp)

            setTitle $ toHtml $ postTitle sp
            $(widgetFile "post")

--------------------------------------------------------------------------------
showAddr :: SockAddr -> String
showAddr (SockAddrInet _ addr)      = show addr
showAddr (SockAddrInet6 _ _ addr _) = show addr
showAddr _ = error "insupported showAddr"
