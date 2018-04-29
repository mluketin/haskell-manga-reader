{-# LANGUAGE OverloadedStrings #-}

module ChapterController where

import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy.IO as TIO

import Scraper
import Model



index :: String -> String -> String -> IO Html
index title chapter page = do 
    let chapterUrl = concat ["/m/", title]
    let url = concat [title, "/", chapter, "/", page]
    chapterViewModel <- Scraper.scrapePage url
    
    let jsNextF = "function next(){console.log('next');window.location.href=\"" ++ (nextPageUrl chapterViewModel) ++ "\";}" :: String
    let jsPrevF = "function prev(){console.log('prev');window.location.href=\"" ++ (prevPageUrl chapterViewModel) ++ "\";}" :: String
    --let js = "var wasPressed=false;document.onkeydown=fkey;document.onkeypress=fkey;document.onkeyup=fkey;function fkey(e){e=e||window.event;if(wasPressed){return true;}console.log(e);if(e.keycode==37){prev();} else if(e.keycode==39){next();}}"
    let js = "document.onkeydown=fkey;function fkey(e){e=e||window.event;console.log(e);if(e.code=='ArrowLeft'){prev();} else if(e.code=='ArrowRight'){next();}}"
    let document = docTypeHtml $ do
        H.head $ do
            H.title "Chapter List"
            link ! rel "stylesheet" ! href "http://s1.mangareader.net/sup/styles/1347525063_04bd9f1aeaab6594c400875d749faa3b.css" ! type_ "text/css"
            link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" 
            --H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
            --H.script ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
        body $ do
            H.div ! A.id "container" $ do
                H.div ! A.id "wrapper_header" $ do
                    H.div ! class_ "top" $ do
                        H.div ! class_ "logo" $ do
                            a ! href "/index" $ "MangaReader"
                --H.div ! A.id "topchapter" $ do
                --H.div ! A.id "navi" $ do
                H.div ! class_ "logo" $ do
                    H.div ! class_ "prevnext" $ do
                        H.span ! class_ "prev" $ do
                            a ! (href $ stringValue $ prevPageUrl chapterViewModel) $ "Prev"
                        H.span ! class_ "next" $ do
                            a ! (href $ stringValue $ nextPageUrl chapterViewModel) $ "Next"                    
                H.table ! A.class_ "episode-table" $ do
                    H.tbody $ do
                        H.tr $ do
                            H.td $ do   
                                H.div ! class_ "content-l-ad" $ do
                                    H.div ! A.width "160" ! A.id "adchapterleft" $ do
                                        p ""
                            H.td ! A.width "100" $ do
                                H.div ! A.id "imgholder" $ do 
                                a ! (href $ stringValue $ nextPageUrl chapterViewModel) $ do
                                img ! A.id "img" ! A.name "img" !  src (stringValue $ imgUrl chapterViewModel) 
                            H.td $ do
                                H.div ! class_ "content-r-ad" $ do
                                    H.div ! A.width "160" ! A.id "adchapterright" $ do
                                        p ""
            H.script $ toHtml (jsNextF ++ jsPrevF ++ js)
    return document









    -- index :: String -> String -> String -> IO Html
-- index title chapter page = do 
--     let chapterUrl = concat ["/m/", title]
--     let url = concat [title, "/", chapter, "/", page]
--     page <- Scraper.scrapePage url
--     let jsNextF = "function next(){console.log('next');window.location.href=\"" ++ (last page) ++ "\";}" :: String
--     let jsPrevF = "function prev(){console.log('prev');window.location.href=\"" ++ (last page) ++ "\";}" :: String
--     --let js = "var wasPressed=false;document.onkeydown=fkey;document.onkeypress=fkey;document.onkeyup=fkey;function fkey(e){e=e||window.event;if(wasPressed){return true;}console.log(e);if(e.keycode==37){prev();} else if(e.keycode==39){next();}}"
--     let js = "document.onkeydown=fkey;function fkey(e){e=e||window.event;console.log(e);if(e.code=='ArrowLeft'){prev();} else if(e.code=='ArrowRight'){next();}}"
--     let document = docTypeHtml $ do
--         H.head $ do
--             H.title "Chapter List"
--             link ! rel "stylesheet" ! href "http://s1.mangareader.net/sup/styles/1347525063_04bd9f1aeaab6594c400875d749faa3b.css" ! type_ "text/css"
--             link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" 
--             --H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
--             --H.script ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
--         body $ do
--             H.div ! A.id "container" $ do
--                 H.div ! A.id "wrapper_header" $ do
--                     H.div ! class_ "top" $ do
--                         H.div ! class_ "logo" $ do
--                             a ! href "/index" $ "MangaReader"
--                 H.div ! A.id "topchapter" $ do
--                     H.div ! A.id "navi" $ do
--                         H.div ! class_ "prevnext" $ do
--                             H.span ! class_ "prev" $ do
--                                 a ! (href $ stringValue $ Prelude.head $ tail page) $ "Prev"
--                             H.span ! class_ "next" $ do
--                                 a ! (href $ stringValue $ last page) $ "Next"                    
--                 H.table ! A.class_ "episode-table" $ do
--                     H.tbody $ do
--                         H.tr $ do
--                             H.td $ do
--                                 H.div ! class_ "content-l-ad" $ do
--                                     H.div ! A.width "160" ! A.id "adchapterleft" $ do
--                                         p ""
--                             H.td ! A.width "100" $ do
--                                 H.div ! A.id "imgholder" $ do 
--                                 a ! (href $ stringValue $ last page) $ do
--                                 img ! A.id "img" ! A.name "img" !  src (stringValue $ Prelude.head page) 
--                             H.td $ do
--                                 H.div ! class_ "content-r-ad" $ do
--                                     H.div ! A.width "160" ! A.id "adchapterright" $ do
--                                         p ""
--             H.script $ toHtml (jsNextF ++ jsPrevF ++ js)
--     return document