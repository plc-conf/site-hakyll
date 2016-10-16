--------------------------------------------------------------------------------
{-# LANGUAGE Arrows             #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main (main) where


--------------------------------------------------------------------------------
import           Data.Monoid      ((<>))
import           Prelude          hiding (id)
import           Filesystem.Path  (basename)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = do
    now <- formatTime defaultTimeLocale "%F" <$> getCurrentTime
    hakyllWith config $ do
        -- Static files
        match  ("images/*.jpg"              .||.
                "images/*.png"              .||.
                "images/sfedu-logo-ru.svg"  .||.
                "images/logo-MM.svg"        .||.
                "images/pvs-studio-logo.svg".||.
                "favicon.ico"               .||.
                "files/**"                  .||.
                ".a")                       $ do
            route   idRoute
            compile copyFileCompiler

        -- Compress CSS
        match "css/*" $ do
            route idRoute
            compile compressCssCompiler

        -- Read templates
        match "templates/*" $ compile $ templateCompiler

        -- Render some static pages

        match "contents/*.md" $ do
            route   $ setExtension ".html"
                        `composeRoutes`
                            (gsubRoute "contents/" (const ""))
            compile $ pandocCompiler
                >>= loadAndApplyTemplate
                        "templates/content.html"
                        (defaultContextWithDate now)
                >>= loadAndApplyTemplate
                        "templates/default.html"
                        (field "id" itemBasename <> defaultContext)
                >>= relativizeUrls

        -- Render the 404 page, we don't relativize URL's here.
        match "contents/404.html" $ do
            route $ gsubRoute "contents/" (const "")
            compile $ pandocCompiler
                >>= loadAndApplyTemplate
                        "templates/content.html"
                        (defaultContextWithDate now)
                >>= loadAndApplyTemplate
                        "templates/default.html"
                        defaultContext

--------------------------------------------------------------------------------

itemBasename :: Item a -> Compiler String
itemBasename = return . encodeString
             . basename . decodeString. toFilePath . itemIdentifier

defaultContextWithDate :: String -> Context String
defaultContextWithDate d = constField "renewDate" d <> defaultContext

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh' \
                      \_site/* plc@test.edu.mmcs.sfedu.ru:/http/plc/data/"
    }

