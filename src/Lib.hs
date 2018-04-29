{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where


import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 hiding (main)
import HtmlBuilder
import HomeController
import MangaController
import ChapterController
import Control.Monad.IO.Class (liftIO)

type API = "index" :> Get '[HTML] Html
      :<|> "m" :> Capture "title" String :> Get '[HTML] Html
      :<|> "c" :> Capture "title" String :> Capture "cp" String :> Capture "page" String :> Get '[HTML] Html
      :<|> "c" :> Capture "title" String :> Capture "cp" String :> Get '[HTML] Html

api :: Proxy API
api = Proxy

server :: Server API
server = home
    :<|> m
    :<|> c
    :<|> c1

  where home            = liftIO HomeController.index
        m title         = liftIO $ MangaController.index title
        c title cp page = liftIO  $ ChapterController.index title cp page
        c1 title cp     = liftIO  $ ChapterController.index title cp "1"
 
app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8081 app

