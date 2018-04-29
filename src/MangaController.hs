{-# LANGUAGE OverloadedStrings #-}

module MangaController where

import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy.IO as TIO

import Scraper


index :: String -> IO Html
index title = do
    chapters <- Scraper.scrapeChapters title
    putStrLn $ show $ length chapters
    let document = docTypeHtml $ do
        H.head $ do
            link ! rel "stylesheet" ! href "http://s1.mangareader.net/sup/styles/1347525063_328bd26e9f7d1e9886ae61ce3961bcb9.css" ! type_ "text/css"
            H.title "Chapter List"
        H.body $ do
            H.div ! A.id "container" $ do
                H.div ! A.id "wrapper_header" $ do
                    H.div ! class_ "top" $ do
                        H.div ! class_ "logo" $ do
                            a ! href "/index" $ "MangaReader"
                H.div ! A.id "wrapper_body" $ do
                    H.header "Chapters"
                    H.div ! A.id "chapterlist" $ do
                        H.table ! A.id "listing" $ do
                            H.tbody $
                                ul $ forM_  chapters (\x -> tr $ td $ a ! (href $ stringValue $ fst x) $ (toHtml $ snd x))
    return document

-- index :: String -> IO Html
-- index title = do
--     chapters <- Scraper.scrapeChapters title
--     putStrLn $ show $ length chapters
--     let document = docTypeHtml $ do
--         H.head $ do
--             link ! rel "stylesheet" ! href "http://s1.mangareader.net/sup/styles/1347525063_328bd26e9f7d1e9886ae61ce3961bcb9.css" ! type_ "text/css"
--             H.title "Chapter List"
--         H.body $ do
--             H.div ! A.id "container" $ do
--                 H.div ! A.id "wrapper_header" $ do
--                     H.div ! class_ "top" $ do
--                         H.div ! class_ "logo" $ do
--                             a ! href "/index" $ "MangaReader"
--                 H.div ! A.id "wrapper_body" $ do
--                     H.header "Chapters"
--                     H.div ! A.id "chapterlist" $ do
--                         H.table ! A.id "listing" $ do
--                             H.tbody $
--                                 ul $ forM_  chapters (\x -> tr $ td $ a ! (href $ stringValue $ fst x) $ (toHtml $ snd x))
--     return document
    

