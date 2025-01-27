--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options
import           Text.Pandoc.Highlighting (zenburn, styleToCss)


root :: String
root = "https://gwydd.ch"

pandocMathCompiler =
    let mathExtensions    = extensionsFromList [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions     = defaultExtensions <> mathExtensions
        writerOptions     = defaultHakyllWriterOptions {
                              writerExtensions = newExtensions,
                              writerHTMLMathMethod = MathJax ""
                            }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith defaultConfiguration {destinationDirectory = "docs"} $ do
    
    match "templates/*" $ compile templateBodyCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    
    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match "articles/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/article.html"    articleCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" articleCtx
            >>= relativizeUrls
    
    match "notes/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/note.html"  noteCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" noteCtx
            >>= relativizeUrls

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss zenburn

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let articlesCtx =
                    listField "articles" articleCtx (return articles) `mappend`
                    constField "title" "Articles"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/articles.html" articlesCtx
                >>= loadAndApplyTemplate "templates/default.html" articlesCtx
                >>= relativizeUrls
    
    create ["notes.html"] $ do
        route idRoute
        compile $ do
            notes <- recentFirst =<< loadAll "notes/*"
            let notesCtx =
                    listField "notes" noteCtx (return notes) `mappend`
                    constField "title" "Notes"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/notes.html" notesCtx
                >>= loadAndApplyTemplate "templates/default.html" notesCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let indexCtx =
                    listField "articles" articleCtx (return articles) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAllSnapshots "articles/*" "content"
            let feedCtx = articleCtx `mappend` bodyField "description"
            renderAtom feedConfig feedCtx articles
  

--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Gwydd's Blog"
    , feedDescription = "Latest articles from Gwydd's blog"
    , feedAuthorName  = "Gwydd"
    , feedAuthorEmail = "me@gwydd.ch"
    , feedRoot        = root
    }
--------------------------------------------------------------------------------   
articleCtx :: Context String
articleCtx =
    constField "root" root      <>
    dateField "date" "%b %d"    <>
    defaultContext
--------------------------------------------------------------------------------
noteCtx :: Context String
noteCtx =
    constField "root" root      <>
    dateField "date" "%b %d"    <>
    defaultContext
--------------------------------------------------------------------------------