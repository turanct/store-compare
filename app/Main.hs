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

type StoreAPI = "product" :> Capture "name" String  :> Get '[JSON] [Product]

server :: Server StoreAPI
server = productCompare
  where productCompare :: String -> Handler [Product]
        productCompare name = do
          products <- liftIO $ queryThomann name
          return products

storeAPI :: Proxy StoreAPI
storeAPI = Proxy

app :: Application
app = serve storeAPI server

main :: IO ()
main = run 8080 app
