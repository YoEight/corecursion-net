module Render.Html
    ( Html
    , markdownToHtml
    , syntaxHighlightingCss
    ) where

--------------------------------------------------------------------------------
import Prelude

--------------------------------------------------------------------------------
import Data.Text as T
import Text.Blaze.Html
import Text.Highlighting.Kate (haddock, styleToCss)
import Text.Pandoc

--------------------------------------------------------------------------------
readerOpts :: ReaderOptions
readerOpts = def { readerExtensions = githubMarkdownExtensions }

--------------------------------------------------------------------------------
writerOpts :: WriterOptions
writerOpts = def { writerHighlight      = True
                 , writerHighlightStyle = haddock
                 }

--------------------------------------------------------------------------------
readContentMarkdown :: Text -> Pandoc
readContentMarkdown txt =
    case readMarkdown readerOpts $ T.unpack txt of
        Left e    -> error $ show e
        Right doc -> doc

--------------------------------------------------------------------------------
syntaxHighlightingCss :: String
syntaxHighlightingCss = styleToCss haddock

--------------------------------------------------------------------------------
getHtml :: Pandoc -> Html
getHtml = writeHtml writerOpts

--------------------------------------------------------------------------------
markdownToHtml :: Text -> Html
markdownToHtml txt = writeHtml writerOpts doc
  where
    doc = readContentMarkdown txt
