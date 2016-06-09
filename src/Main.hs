--------------------------------------------------------------------------------
{-# LANGUAGE Arrows             #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main (main) where


--------------------------------------------------------------------------------
import           Data.Monoid     ((<>))
import           Prelude         hiding (id)
import           Filesystem.Path (basename)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = hakyllWith config $ do
    -- Static files
    match ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif" .||.
            "favicon.ico" .||. "files/**") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    -- Render some static pages
    match (fromList pages) $ do
        route   $ setExtension ".html" `composeRoutes` (gsubRoute "contents/" (const "")) 
                    
        compile $ pandocCompiler
            >>= loadAndApplyTemplate 
                    "templates/content.html" 
                    defaultContext
            >>= loadAndApplyTemplate 
                    "templates/default.html"
                    (field "id" itemBasename <> defaultContext)
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate 
                    "templates/content.html" 
                    defaultContext
            >>= loadAndApplyTemplate 
                    "templates/default.html" 
                    defaultContext

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx

    -- CV as HTML
{-
    match "cv.markdown" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/cv.html"      defaultContext
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- CV as PDF
    match "cv.markdown" $ version "pdf" $ do
        route   $ setExtension ".pdf"
        compile $ do getResourceBody
            >>= readPandoc
            >>= (return . fmap writeXeTex)
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= xelatex
-}
   where
    pages =
        [ "contents/contact.md"
        , "contents/index.md"
        , "contents/organizers.md"
        , "contents/topics.md"
        ]

itemBasename :: Item a -> Compiler String
itemBasename = return . encodeString 
             . basename . decodeString. toFilePath . itemIdentifier

--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh' \
                      \_site/* plc@test.edu.mmcs.sfedu.ru:/http/plc/data/"
    }


--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "jaspervdj - " ++ title
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedAuthorEmail = "jaspervdj@gmail.com"
    , feedRoot        = "http://jaspervdj.be"
    }


