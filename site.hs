--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import Hakyll
    ( getResourceBody,
      makeItem,
      saveSnapshot,
      loadAll,
      loadAllSnapshots,
      defaultConfiguration,
      copyFileCompiler,
      fromList,
      idRoute,
      setExtension,
      compile,
      create,
      match,
      route,
      hakyllWith,
      compressCssCompiler,
      renderAtom,
      relativizeUrls,
      pandocCompiler,
      bodyField,
      constField,
      dateField,
      defaultContext,
      listField,
      applyAsTemplate,
      loadAndApplyTemplate,
      templateBodyCompiler,
      recentFirst,
      Configuration(destinationDirectory),
      FeedConfiguration(..),
      Context )

root :: String
root = "https://gwydd.se"
--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith defaultConfiguration {destinationDirectory = "docs"} $ do

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
    
    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
    
    match "reviews/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/review.html"  reviewCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" reviewCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
    
    create ["reviews.html"] $ do
        route idRoute
        compile $ do
            reviews <- recentFirst =<< loadAll "reviews/*"
            let reviewsCtx =
                    listField "reviews" reviewCtx (return reviews) `mappend`
                    constField "title" "Reviews"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/reviews.html" reviewsCtx
                >>= loadAndApplyTemplate "templates/default.html" reviewsCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let feedCtx = postCtx `mappend` bodyField "description"
            renderAtom feedConfig feedCtx posts
  

    match "templates/*" $ compile templateBodyCompiler
--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Gwydd's Blog"
    , feedDescription = "Latest posts from Gwydd's blog"
    , feedAuthorName  = "Gwydd"
    , feedAuthorEmail = "me@gwydd.se"
    , feedRoot        = root
    }
--------------------------------------------------------------------------------   
postCtx :: Context String
postCtx =
    constField "root" root         <>
    dateField "date" "%b %d"    <>
    defaultContext
--------------------------------------------------------------------------------
reviewCtx :: Context String
reviewCtx = 
    constField "root" root <>
    dateField "date" "%b %d" <>
    defaultContext