{-# LANGUAGE DeriveGeneric #-}

module Product where

import GHC.Generics
import Data.Aeson

type ProductName = String

data Product = Product
  { name :: ProductName
  , price :: String
  , url :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Product
