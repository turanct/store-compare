module Client where

import Text.XML.Cursor

type Request = String

class Monad m => Client m where
  get :: Request -> m Cursor
