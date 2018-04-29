{-# LANGUAGE OverloadedStrings #-}

module HtmlBuilder where

import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy.IO as TIO


numbers :: Int -> Html
numbers n = docTypeHtml $ do
    H.head $ do
        H.title "Natural numbers"
    body $ do
        p "A list of natural numbers:"
        ul $ forM_ [1 .. n] (li . toHtml)

notes :: Html
notes = docTypeHtml $ do
    H.head $ do
        H.title "John' s Page"
    body $ do
        p "Hello World!"


notes2 mangaTitle = docTypeHtml $ do
    H.head $ do
        H.title "John' s Page"
    body $ do
        p mangaTitle


main1 = TIO.putStr $ renderHtml notes
main2 = TIO.putStr $ renderHtml $ numbers 5


{-
data Attribute = Attribute 
  { attrName  :: String
  , attrValue :: String
  }

data Tag = Tag 
  { tagName :: String
  , attrs   :: [Attribute]
  , tags    :: [Tag]
  } 

  
instance Show Attribute where
  show attr = concat [attrName attr, "=\"", attrValue attr, "\""]  
  -}
  {-
instance Show Tag where
  show tag = concat ["<", tagName tag, " ", intercalate " " (showAttrs attrs tag), " >", "</",tagName tag, ">"]
    where showAttrs attrs
-}

createElementA :: String -> String -> String
createElementA href text = "<a href=\"" ++ href ++ "\">" ++ text ++ "</a>"

