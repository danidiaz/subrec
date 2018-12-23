{-# LANGUAGE DataKinds, 
			 DeriveGeneric,
             TypeApplications
             #-}
module Subrec.Examples (
    module Subrec.Examples,
    Data.Aeson.eitherDecodeStrict'
) where

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified GHC.Generics as GHC
import           Generics.SOP
import           Char8 (ByteString)
import qualified Char8
import           Subrec

data Person = Person { name :: String, age :: Int, salary :: Int } deriving (Show,GHC.Generic)
instance Generic Person
instance HasDatatypeInfo Person

getAge :: Subrec '["name","age"] Person -> Int
getAge = subGetField (Proxy @"age")

type RestrictedPerson1 = Subrec ["name","age"] Person

type RestrictedPerson2 = Subrec '["name"] Person

personString1 :: ByteString
personString1 = Char8.pack "{ \"name\" : \"Foo\", \"age\" : 81 }"

personString2 :: ByteString
personString2 = Char8.pack "{ \"name\" : \"Foo\" }"

