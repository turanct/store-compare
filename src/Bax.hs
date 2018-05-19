{-# LANGUAGE OverloadedStrings #-}

module Bax where

import Product

import qualified Network.HTTP.Simple as N
import Text.HTML.DOM
import Text.XML.Cursor
import qualified Text.XML
import qualified Data.Text as T
import qualified Data.Char as C

queryBax :: String -> IO [Product]
queryBax name = do
  request <- N.parseRequest $ "GET https://www.bax-shop.nl/producten.html?keyword=" ++ name
  response <- N.httpLBS request
  let document = parseLBS $ N.getResponseBody response
  let products = productsFromDocument document
  return products

productsFromDocument :: Text.XML.Document -> [Product]
productsFromDocument document = matchedProducts
  where
    findProducts = element "div" >=> attributeIs "class" "result "
    findPrice = element "span" >=> attributeIs "class" "voor-prijs"
    findLink = element "a" >=> attributeIs "class" "trackable-product"
    cursor = fromDocument document
    matches = cursor $// findProducts
    matchedProducts = map parseProduct matches
    parseProduct c = Product (parseTitle c) (parsePrice c) (parseUrl c)
    parseTitle c = T.unpack . T.concat $ (c $/ findLink) >>= attribute "title"
    parsePrice c = takeWhile C.isDigit $ filter (/= ',') $ dropWhile (not . C.isDigit) $ T.unpack . T.concat $ c $// findPrice &// content
    parseUrl c = T.unpack . T.concat $ (c $/ findLink) >>= attribute "href"
