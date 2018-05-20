{-# LANGUAGE DeriveGeneric #-}

module Store where

import Product
import GHC.Generics
import Data.Aeson

data Store = Store
  { name :: String
  , products :: [Product]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Store
