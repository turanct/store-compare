{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Product
import Keymusic
import Thomann
import Andertons

import Prelude
import Control.Monad.IO.Class
import Servant
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp

import Control.Concurrent.Async

type StoreAPI = "product" :> Capture "name" String  :> Get '[JSON] [(String, [Product])]

server :: Server StoreAPI
server = productCompare
  where productCompare :: String -> Handler [(String, [Product])]
        productCompare searchName = liftIO $ mapConcurrently (productsForStore searchName) stores

        productsForStore searchName (store, query) = do
          products <- query searchName
          return (store, products)
        stores = [ ("Keymusic", queryKeymusic)
                 , ("Thomann", queryThomann)
                 , ("Andertons", queryAndertons)
                 ]

storeAPI :: Proxy StoreAPI
storeAPI = Proxy

app :: Application
app = serve storeAPI server

main :: IO ()
main = run 8080 app
