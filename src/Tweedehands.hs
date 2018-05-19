{-# LANGUAGE OverloadedStrings #-}

module Tweedehands where

import Product

import qualified Network.HTTP.Simple as N
import Text.HTML.DOM
import Text.XML.Cursor
import qualified Text.XML
import qualified Data.Text as T
import qualified Data.Char as C

queryTweedehands :: String -> IO [Product]
queryTweedehands name = do
  request <- N.parseRequest $ "GET https://www.2dehands.be/markt/2/" ++ name ++ "/"
  response <- N.httpLBS request
  let document = parseLBS $ N.getResponseBody response
  let products = productsFromDocument document
  return products

productsFromDocument :: Text.XML.Document -> [Product]
productsFromDocument document = matchedProducts
  where
    findProducts = element "article" >=> attributeIs "itemtype" "http://schema.org/Product"
    findPrice = element "div" >=> attributeIs "itemprop" "price"
    findLink = element "a" >=> attributeIs "class" "listed-adv-item-link"
    cursor = fromDocument document
    matches = cursor $// findProducts
    matchedProducts = map parseProduct matches
    parseProduct c = Product (parseTitle c) (parsePrice c) (parseUrl c)
    parseTitle c = T.unpack . T.strip . T.concat $ c $// findLink &// content
    parsePrice c = takeWhile C.isDigit $ filter (/= '.') $ dropWhile (not . C.isDigit) $ T.unpack . T.concat $ c $// findPrice &// content
    parseUrl c = T.unpack . T.concat $ (c $// findLink) >>= attribute "href"
