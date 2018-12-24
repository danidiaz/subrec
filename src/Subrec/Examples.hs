{-| 
>>> :set -XTypeApplications -XDataKinds -XOverloadedStrings
 -}
{-# LANGUAGE DataKinds, 
             DeriveGeneric,
             TypeApplications
             #-}
module Subrec.Examples (
    module Subrec.Examples,
    module Data.Proxy,
    Data.Aeson.eitherDecodeStrict',
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

personString1 :: ByteString
personString1 = Char8.pack "{ \"name\" : \"Foo\", \"age\" : 81 }"

{-|
>>> eitherDecodeStrict' @RestrictedPerson1 "{ \"name\" : \"Foo\", \"age\" : 81 }" 
 
>>> subGetField (Proxy @"name") person1

>>> subGetField (Proxy @"age") person1
 -}
person1 :: RestrictedPerson1 
person1 = 
    let Right p = eitherDecodeStrict' personString1 
     in p

type RestrictedPerson2 = Subrec '["name"] Person

personString2 :: ByteString
personString2 = Char8.pack "{ \"name\" : \"Foo\" }"

{-|
>>> eitherDecodeStrict' @RestrictedPerson2 "{ \"name\" : \"Foo\" }"  

>>> eitherDecodeStrict' @RestrictedPerson1 "{ \"name\" : \"Foo\" }"  

>>> subGetField (Proxy @"name") person2

>>> subGetField (Proxy @"age") person2
 -}
person2 :: RestrictedPerson2 
person2 = 
    let Right p = eitherDecodeStrict' personString2 
     in p

