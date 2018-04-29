{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Text.HTML.Scalpel.Core
import qualified Text.HTML.TagSoup as TagSoup
import Data.Text.Internal
import Model


scrapeTest :: (Eq a, Show a) => String -> Scraper String a -> Maybe a
scrapeTest html scraper = scrape scraper (TagSoup.parseTags html)

t1 = scrapeTest "<a>foo</a>" (htmls ("a" @: []))

getValue :: Maybe[String] -> [String]
getValue maybeVal = case maybeVal of
  Just val -> val
  Nothing  -> [] 

cleanHref :: String -> String
cleanHref = reverse . drop 2 . reverse . drop 2
  
scrapeTitles :: IO [(String, String)]
scrapeTitles = do
  r <- get "http://www.mangareader.net/alphabetical"
  let bodyString = r ^. responseBody 
  let str = show bodyString
  let scrapedTitleLinks = scrapeTest str (attrs "href" ("div" @: [hasClass "\\\"series_alpha\\\""] // "ul" // "a")) --(htmls ("ul" @: [hasClass "\\\"series_alpha\\\""])) --
  let titleLinks = map (\x -> "/m/" ++ (reverse $ drop 2 $ reverse $ drop 3 x)) $ getValue scrapedTitleLinks 
  let scrapedTitleText = scrapeTest str (texts ("div" @: [hasClass "\\\"series_alpha\\\""] // "ul" // "a"))  
  let titleTexts = getValue scrapedTitleText
  return $ zip titleLinks titleTexts
  
  
scrapeChapters :: String -> IO [(String, String)]
scrapeChapters title = do
  r <- get $ "http://www.mangareader.net/" ++ title
  let bodyString = r ^. responseBody 
  let str = show bodyString
  let scrapedChapterLinks = scrapeTest str (attrs "href" ("table" @: ["id" @= "\\\"listing\\\""] // "a")) 
  let chapterLinks = map (\x -> "/c/" ++ (reverse $ drop 2 $ reverse $ drop 3 x) ++ "/1") $ getValue scrapedChapterLinks 
  let scrapedChapterText = scrapeTest str (texts ("table" @: ["id" @= "\\\"listing\\\""] // "a")) 
  let chapterTexts = getValue scrapedChapterText
  --let scrapedChapterNames = scrapeTest str (texts ("table" @: ["id" @= "\\\"listing\\\""] // "td")) 
  return $ zip chapterLinks chapterTexts

scrapePage :: String -> IO  ChapterViewModel--[String]
scrapePage pageURL = do
  r <- get $ "http://www.mangareader.net/" ++ pageURL
  let bodyString = r ^. responseBody 
  let str = show bodyString
  let navigationScraped = scrapeTest str (attrs "href" ("div" @: ["id" @= "\\\"navi\\\""] // "a")) 
  let navigationClean = getValue navigationScraped

  let prevLink = "/c" ++ (cleanHref $ head navigationClean)
  let nextLink = "/c" ++ (cleanHref $ last navigationClean)
  
  -- putStrLn $ show prevLink
  -- putStrLn $ show nextLink

  let imgLinkScraped = scrapeTest str (attrs "src" ("div" @: ["id" @= "\\\"imgholder\\\""] // "img")) 
  let imgLink = cleanHref $ head $ getValue imgLinkScraped
  -- putStrLn imgLink
  return $ ChapterViewModel imgLink prevLink nextLink
  --return [imgLink, prevLink, nextLink]
