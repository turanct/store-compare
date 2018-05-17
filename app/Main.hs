{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Product
import Keymusic
import Thomann

import Prelude
import Control.Monad.IO.Class
import Servant
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp

type StoreAPI = "product" :> Capture "name" String  :> Get '[JSON] [(String, [Product])]

server :: Server StoreAPI
server = productCompare
  where productCompare :: String -> Handler [(String, [Product])]
        productCompare searchName = mapM (liftIO . productsForStore searchName) stores

        productsForStore searchName (store, query) = do
          products <- query searchName
          return (store, products)
        stores = [ ("KeyMusic", queryKeymusic)
                 , ("Thomann", queryThomann)
                 ]

storeAPI :: Proxy StoreAPI
storeAPI = Proxy

app :: Application
app = serve storeAPI server

main :: IO ()
main = run 8080 app
