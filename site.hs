-- # TODO
-- * consider linking post author names to profile pages, since the author tags page is right below
-- * fix site.hs to not require dates on recipes?
-- * use tags as headings on recipes.html
-- * create an attractive template for recipes, plus a button to switch from side-by-side vs. vertical format
-- * add a bottom margin? styles are specified for a footer but does it exist?
-- * create a default tag for recipes and posts
-- * reimplement recipe step numbering, notes styling

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Aeson
import           Data.Semigroup
import           Hakyll
import           Text.Pandoc

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ( "images/**" .||. "docs/**" .||. "slides/**" ) $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["cbieber.md", "bergey.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    --build up tags
    tags <- buildTags ("posts/*" .||. "recipes/*") (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title                  <>
                    listField "posts" postCtx (return posts)  <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "recipes/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/recipe.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["recipes.html"] $ do
        route idRoute
        compile $ do
            recipes <- recentFirst =<< loadAll "recipes/*"
            let recipeCtx =
                    listField "recipes" postCtx (return recipes)  <>
                    constField "title" "Recipes"                  <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/cookbook.html" recipeCtx
                >>= loadAndApplyTemplate "templates/default.html" recipeCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
          indexCtx <- teasers

          getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
teasers :: Compiler (Context a)
teasers = do
  posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
  tpl <- loadBody "templates/teaser.html"
  heads <- applyTemplateList tpl (teaserField "teaser" "content" <> postCtx) $ take 3 posts
  return $ constField "posts" heads

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx
