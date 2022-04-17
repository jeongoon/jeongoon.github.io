--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Data.List (intersperse)
import           Hakyll

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  , previewPort          = 5000
  }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- 20Mar2022 trying to use tag
    -- ref: https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    -- build up tags
    tags <- buildTagsWith getTagsByKeywords -- 17Apr2022 using keywords field
           "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" <> tag <> "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" (postCtxWithTags tags)
                                                  (return posts)
                      `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"
                (postCtxWithTags tags)

            >>= loadAndApplyTemplate "templates/default.html"
                (postCtxWithTags tags)

            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
-- Some Additional features:

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
  tagsFieldWithKeywords "tags" tags `mappend` postCtx
  where
    tagsFieldWithKeywords =
      tagsFieldWith getTagsByKeywords simpleRenderLink
      (mconcat . intersperse ", ")

-- 17Apr2022: I used to use keywords and tags but I found it they are doing
-- the same thing. so I tried to get tags from keywords field
getTagsByKeywords :: MonadMetadata m => Identifier -> m[String]
getTagsByKeywords = getTagsByField "keywords"


-- copy from: https://www.stackage.org/haddock/lts-19.4/hakyll-4.15.1.1/src/Hakyll.Web.Tags.html
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) = Just $
    H.a ! A.title (H.stringValue ("All pages tagged '"++tag++"'."))
        ! A.href (toValue $ toUrl filePath)
        $ toHtml tag
