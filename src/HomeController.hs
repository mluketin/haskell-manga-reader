{-# LANGUAGE OverloadedStrings #-}

module HomeController where

import Data.Char

import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy.IO as TIO

import Scraper


index :: IO Html
index = do 
  --titles (href, text)
  titles <- Scraper.scrapeTitles
  let alphabetLeft  = "ABCDEFGHIJK" :: String
  let alphabetRight = "LMNOPQRSTUVWXYZ" :: String

  let titlesLeft = filter(\(f,s) -> ((Prelude.head s) `elem` alphabetLeft) || (not $ isAlpha $ Prelude.head s)) titles
  let titlesRight = filter(\(f,s) -> (Prelude.head s) `elem` alphabetRight) titles

  let document = docTypeHtml $ do
      H.head $ do
          link ! rel "stylesheet" ! href "http://s2.mangareader.net/sup/styles/1347525063_d8ef06ddf0da023cd7cf37711b8d7f9f.css" ! type_ "text/css"
          H.title "Manga List"
      body $ do
        H.div ! A.id "container" $ do
            H.div ! A.id "wrapper_header" $ do
                H.div ! class_ "top" $ do
                    H.div ! class_ "logo" $ do
                        a ! href "/index" $ "MangaReader"
            H.div ! A.id "wrapper_body" $ do
                H.div ! class_ "content_bloc2" $ do
                    H.div ! class_ "clear" $ do p ""
                    H.div ! class_ "series_col" $ do
                        H.div ! class_ "series_alpha" $  
                            ul $ forM_  titlesLeft (\x -> li $ a ! (href $ stringValue $ fst x) $ (toHtml $ snd x))
                    H.div ! class_ "series_col" $ do
                        H.div ! class_ "series_alpha" $  
                            ul $ forM_  titlesRight (\x -> li $ a ! (href $ stringValue $ fst x) $ (toHtml $ snd x))
  return document
