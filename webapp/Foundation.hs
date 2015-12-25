module Foundation where

import Import.NoFoundation hiding (hash)
import Aggregate.Authors
import Aggregate.Post
import Aggregate.Posts
import Aggregate.Stats
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.GoogleEmail2
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appPosts       :: Posts
    , appStats       :: Stats
    , appAuthors     :: Authors
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        muser <- maybeAuthId

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScript $ StaticR js_jquery_min_js
            addScript $ StaticR js_bootstrap_min_js
            addStylesheet $ StaticR screen_css
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_clean_blog_css
            addStylesheetRemote "http://maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css"
            addStylesheetRemote "http://fonts.googleapis.com/css?family=Lora:400,700,400italic,700italic"
            addStylesheetRemote "http://fonts.googleapis.com/css?family=Open+Sans:300italic,400italic,600italic,700italic,800italic,400,300,600,700,800"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized AdminHomeR _ = isAuthenticated
    isAuthorized AdminCreatePostR _ = isAuthenticated
    isAuthorized (AdminPostR _) _ = isAuthenticated
    isAuthorized (AdminPublishPostR _) _ = isAuthenticated
    isAuthorized (AdminUnpublishPostR _) _ = isAuthenticated
    isAuthorized (AdminPreviewPostR _) _ = isAuthenticated
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

instance YesodAuth App where
    type AuthId App = AuthorId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authPlugins app = [authGoogleEmailSaveToken clientID secret]
      where
        g        = appGoogleAuth $ appSettings app
        clientID = googleClientID g
        secret   = googleSecret g

    authLayout = loginLayout

    getAuthId creds = do
        app <- getYesod
        let login = credsIdent creds
        liftIO $ lookupAuthorId (appAuthors app) login

    maybeAuthId = do
        app  <- getYesod
        skey <- lookupSession credsKey
        case skey of
            Just key -> liftIO $ lookupAuthorId (appAuthors app) key
            _        -> return Nothing

    authHttpManager = getHttpManager

loginLayout :: WidgetT App IO () -> Handler Html
loginLayout w = do
    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_bootstrap_theme_min_css
        addScript $ StaticR js_bootstrap_min_js
        addScript $ StaticR js_jquery_min_js
        w
    withUrlRenderer $(hamletFile "templates/login-layout.hamlet")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

--------------------------------------------------------------------------------
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    authm <- maybeAuthId
    case authm of
        Just _ -> return Authorized
        _      -> return AuthenticationRequired

--------------------------------------------------------------------------------
unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

--------------------------------------------------------------------------------
adminLayout :: WidgetT App IO () -> Handler Html
adminLayout widget = do
    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_bootstrap_theme_min_css
        addStylesheet $ StaticR css_simple_sidebar_css
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_min_js
        $(widgetFile "admin-layout")
    withUrlRenderer $(hamletFile "templates/admin-layout-wrapper.hamlet")

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
