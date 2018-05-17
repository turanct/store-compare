{-# LANGUAGE OverloadedStrings #-}

module Keymusic where

import Product

import qualified Network.HTTP.Simple as N
import Text.HTML.DOM
import Text.XML.Cursor
import qualified Text.XML
import qualified Data.Text as T
import qualified Data.Char as C

queryKeymusic :: String -> IO [Product]
queryKeymusic name = do
  request <- N.parseRequest $ "GET https://www.keymusic.com/be/zoeken?q=" ++ name
  response <- N.httpLBS request
  let document = parseLBS $ N.getResponseBody response
  let products = productsFromDocument document
  return products

productsFromDocument :: Text.XML.Document -> [Product]
productsFromDocument document = matchedProducts
  where
    findProducts = element "div" >=> attributeIs "class" "product"
    findPrice = element "span" >=> attributeIs "class" "price-label"
    findLink = element "a" >=> attributeIs "class" "product-link clearfix"
    cursor = fromDocument document
    matches = cursor $// findProducts
    matchedProducts = map parseProduct matches
    parseProduct c = Product (parseTitle c) (parsePrice c) (urlPrefix ++ parseUrl c)
    parseTitle c = T.unpack . T.concat $ c $| attribute "title"
    parsePrice c = takeWhile C.isDigit $ dropWhile (not . C.isDigit) $ T.unpack . T.concat $ c $// findPrice &// content
    parseUrl c = T.unpack . T.concat $ (c $// findLink) >>= attribute "href"
    urlPrefix = "https://www.keymusic.com"
