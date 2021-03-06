{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Product
import Store
import Keymusic
import Thomann
import Bax
import Andertons
import Tweedehands

import Prelude
import Control.Monad.IO.Class
import Servant
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

import Control.Concurrent.Async

type StoreAPI = "product" :> Capture "name" String  :> Get '[JSON] [Store]
           :<|> Raw

server :: Server StoreAPI
server = productCompare :<|> serveDirectoryWebApp "frontend-compiled"
  where productCompare :: String -> Handler [Store]
        productCompare searchName = liftIO $ mapConcurrently (productsForStore searchName) stores

        productsForStore searchName (store, query) = Store store <$> query searchName

        stores = [ ("Keymusic", queryKeymusic)
                 , ("Thomann", queryThomann)
                 , ("Bax-shop", queryBax)
                 , ("Andertons", queryAndertons)
                 , ("Tweedehands", queryTweedehands)
                 ]

storeAPI :: Proxy StoreAPI
storeAPI = Proxy

app :: Application
app = simpleCors (serve storeAPI server)

main :: IO ()
main = run 8080 app
