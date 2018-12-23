{-# LANGUAGE DataKinds, 
			 DeriveGeneric 
             #-}
module Subrec.Examples where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified GHC.Generics as GHC
import           Generics.SOP

import           Subrec

data Person = Person { name :: String, age :: Int } deriving (Show,GHC.Generic)
instance Generic Person
instance HasDatatypeInfo Person

personParser :: Value -> Parser (Subrec '["name"] Person)
personParser = parseJSON
