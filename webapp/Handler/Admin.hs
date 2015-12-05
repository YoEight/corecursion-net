--------------------------------------------------------------------------------
module Handler.Admin where

--------------------------------------------------------------------------------
import qualified Data.Text as T
import           Text.Cassius
import           Text.Julius

--------------------------------------------------------------------------------
import Handler.Common
import Import hiding (parseDate)
import Repository

--------------------------------------------------------------------------------
getAdminHomeR :: Handler Html
getAdminHomeR = do
    posts       <- getPosts
    isPublished <- isPublishedF
    adminLayout $ do
        setTitle "Admin - Home"
        $(widgetFile "admin-post-list")

--------------------------------------------------------------------------------
getAdminPostR :: PostId -> Handler Html
getAdminPostR postid = do
    pm <- getPost postid
    case pm of
        Nothing -> notFound
        Just p  -> do
            let formId = "form-id" :: Text
            pc <- widgetToPageContent $ do
                deleteModalId    <- newIdent
                publishModalId   <- newIdent
                unpublishModalId <- newIdent
                postDateId       <- newIdent

                $(widgetFile "update-form-action")

            adminLayout $ do
                setTitle "Admin - Update post"

                let actionTitle = "Update post" :: Text
                    title       = _postTitle p
                    tags        = unwords $ _postTags p
                    summary     = _postSummary p
                    content     = _postContent p
                    actionLabel = "Update" :: Text

                toWidgetHead $ pageHead pc
                $(widgetFile "admin-post-form")

--------------------------------------------------------------------------------
getAdminCreatePostR :: Handler Html
getAdminCreatePostR = do
    pc <- widgetToPageContent mempty
    adminLayout $ do
        formId <- newIdent
        setTitle "Admin - Create post"
        let actionTitle = "Create post" :: Text
            title       = ""            :: Text
            tags        = ""            :: Text
            summary     = ""            :: Text
            content     = ""            :: Text
            actionLabel = "Create"      :: Text

        toWidget $(juliusFile "templates/admin-create-form.julius")
        $(widgetFile "admin-post-form")

--------------------------------------------------------------------------------
postAdminCreatePostR :: Handler TypedContent
postAdminCreatePostR = do
    t_m  <- lookupPostParam "post-title"
    tg_m <- lookupPostParam "post-tags"
    s_m  <- lookupPostParam "post-summary"
    c_m  <- lookupPostParam "post-content"
    let tags = maybe [] words tg_m
        s    = fromMaybe "" s_m
        c    = maybe "" (T.replace "\r\n" "\n") c_m
        res  = do
            t <- t_m
            return $ CreatePost t c s tags
    case res of
        Nothing  -> invalidArgs []
        Just cmd -> do
            executeContentCommand cmd
            sendResponseStatus status201 ()

--------------------------------------------------------------------------------
postAdminPostR :: PostId -> Handler TypedContent
postAdminPostR postid = do
    pm <- getPost postid
    case pm of
        Nothing -> notFound
        Just p  -> do
            cmds <- extractPostCommands p
            executePostCommands postid cmds
            sendResponseStatus status204 ()

--------------------------------------------------------------------------------
extractPostCommands :: Post -> Handler [PostCommand]
extractPostCommands p = titleState
  where
    titleState = do
        tm  <- lookupPostParam "post-title"
        xs <- tagsState
        case tm of
            Just t | t == _postTitle p -> return xs
                   | otherwise         -> return (SetPostTitle t:xs)
            _ -> return xs

    tagsState = do
        tgsm <- lookupPostParam "post-tags"
        xs   <- summaryState
        case tgsm of
            Just tgs ->
                let ts       = T.words tgs
                    ps       = _postTags p
                    sameSize = length ts == length ps
                    similar  = all (`elem` ps) ts in
                if sameSize && similar
                then return xs
                else return (SetPostTags ts:xs)
            _ -> return xs

    summaryState = do
        sm <- lookupPostParam "post-summary"
        xs <- contentState
        case sm of
            Just s | s == _postSummary p -> return xs
                   | otherwise           -> return (SetPostSummary s:xs)
            _ -> return xs

    contentState = do
        cm <- lookupPostParam "post-content"
        case cm of
            Just c ->
                let c' = T.replace "\r\n" "\n" c in
                if c' == _postContent p
                then return []
                else return [SetPostContent c']
            _ -> return []

--------------------------------------------------------------------------------
deleteAdminPostR :: PostId -> Handler TypedContent
deleteAdminPostR postid = do
    pm <- getPost postid
    case pm of
        Nothing -> notFound
        Just _  -> do
            executeContentCommand (DeletePost postid)
            sendResponseStatus status204 ()

--------------------------------------------------------------------------------
putAdminPublishPostR :: PostId -> Handler TypedContent
putAdminPublishPostR postid = do
    dateTxt <- lookupPostParam "post-date"
    pm      <- getPost postid
    case pm of
        Nothing -> notFound
        Just _  -> do
            let forcedDate = dateTxt >>= parseDate
            executeContentCommand (PublishPost forcedDate postid)
            sendResponseStatus status204 ()

--------------------------------------------------------------------------------
putAdminUnpublishPostR :: PostId -> Handler TypedContent
putAdminUnpublishPostR postid = do
    pm <- getPost postid
    case pm of
        Nothing -> notFound
        Just _  -> do
            executeContentCommand (UnpublishPost postid)
            sendResponseStatus status204 ()

--------------------------------------------------------------------------------
getAdminPreviewPostR :: PostId -> Handler Html
getAdminPreviewPostR postid = do
    pm <- postPreview postid
    case pm of
        Nothing -> notFound
        Just p  -> do
            isPublished <- isPublishedF
            adminLayout $ do
                let date = showDate p
                setTitle "Admin - Post preview"
                toWidget $(cassiusFile "templates/post.cassius")
                $(widgetFile "admin-post-preview")

--------------------------------------------------------------------------------
getAdminAboutR :: Handler Html
getAdminAboutR = do
    content <- aboutPageText
    adminLayout $ do
        formId <- newIdent
        $(widgetFile "admin-about")

--------------------------------------------------------------------------------
postAdminAboutR :: Handler TypedContent
postAdminAboutR = do
    cm <- lookupPostParam "about-content"
    let c = maybe "" (T.replace "\r\n" "\n") cm
    executeContentCommand (SetAbout c)
    sendResponseStatus status204 ()
