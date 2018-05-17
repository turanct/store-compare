{-# LANGUAGE OverloadedStrings #-}

module Thomann where

import Product

import qualified Network.HTTP.Simple as N
import Text.HTML.DOM
import Text.XML.Cursor
import qualified Text.XML
import qualified Data.Text as T
import qualified Data.Char as C

queryThomann :: String -> IO [Product]
queryThomann name = do
  request <- N.parseRequest $ "GET https://www.thomann.de/be/search_dir.html?sw=" ++ name
  response <- N.httpLBS request
  let document = parseLBS $ N.getResponseBody response
  -- print document
  let products = productsFromDocument document
  return products

productsFromDocument :: Text.XML.Document -> [Product]
productsFromDocument document = matchedProducts
  where
    findResults = element "div" >=> attributeIs "id" "defaultResultPage"
    findProducts = element "div" >=> attributeIs "class" "extensible-article list-view compare parent"
    findTitle = element "div" >=> attributeIs "class" "title-block link-group"
    findPrice = element "span" >=> attributeIs "class" "article-basketlink"
    findLink = element "a" >=> attributeIs "class" "article-link link"
    cursor = fromDocument document
    matches = (cursor $// findResults) >>= child >>= findProducts
    matchedProducts = map parseProduct matches
    parseProduct c = Product (parseTitle c) (parsePrice c) (parseUrl c)
    parseTitle c = T.unpack . T.concat $ c $// findTitle &// content
    parsePrice c = takeWhile C.isDigit $ dropWhile (not . C.isDigit) $ T.unpack . T.concat $ c $// findPrice &// content
    parseUrl c = T.unpack . T.concat $ take 1 $ (c $// findLink) >>= attribute "href"
