{-# LANGUAGE DataKinds, 
			 TypeFamilies, 
			 FlexibleContexts, 
			 UndecidableInstances, 
			 DeriveFunctor, 
			 DeriveFoldable, 
			 DeriveTraversable, 
			 KindSignatures, 
			 ConstraintKinds, 
			 TypeOperators, 
			 TypeApplications, 
			 ScopedTypeVariables, 
			 ViewPatterns, 
			 FlexibleInstances, 
			 TypeOperators, 
			 DeriveGeneric, 
			 PolyKinds,
			 ExistentialQuantification,
             MultiParamTypeClasses
             #-}
module Subrec.Internal where

import           Data.Kind
import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Type.Equality (type (==))
import           Map (Map)
import qualified Map 
import           GHC.TypeLits
import qualified GHC.Generics as GHC
import           Generics.SOP
import qualified Generics.SOP.Type.Metadata as M

data Stuff = forall a . Stuff a

newtype Subrec (ns :: [Symbol]) r = Subrec (Map String Stuff)

instance (IsProductType r xs, 
          HasDatatypeInfo r,
          ConstructorOf (DatatypeInfoOf r) ~ c,
          ConstructorFieldNamesOf c ~ ns',
          IsSubset ns ns' ~ True,
          All FromJSON xs) => FromJSON (Subrec ns r) where
    parseJSON _ = undefined

type family ConstructorOf (a :: M.DatatypeInfo) :: M.ConstructorInfo where
    ConstructorOf ('M.ADT moduleName datatypeName '[constructor]) = constructor

type family ConstructorFieldNamesOf (a :: M.ConstructorInfo) :: [Symbol] where
    ConstructorFieldNamesOf ('M.Record constructorName fields) = FieldNamesOf fields

type family FieldNamesOf (a :: [M.FieldInfo]) :: [Symbol] where
    FieldNamesOf '[] = '[]
    FieldNamesOf (('M.FieldInfo n) ': xs) = n ': FieldNamesOf xs

type family IsMember (x :: Symbol) (ys :: [Symbol]) :: Bool where
    IsMember x '[]       = False
    IsMember x (x ': ys) = True
    IsMember x (y ': ys) = IsMember x ys

type family IsSubset (xs :: [Symbol]) (ys :: [Symbol]) :: Bool where
    IsSubset '[]       _  = True
    IsSubset (x ': xs) ys = And' (IsMember x ys) (IsSubset xs ys)  

type family And' (b::Bool) (b'::Bool) :: Bool where
    And' True True = True
    And' _    _    = False

data Person = Person { name :: String, age :: Int } deriving (Show,GHC.Generic)
instance Generic Person
instance HasDatatypeInfo Person

personParser :: Value -> Parser (Subrec '["name"] Person)
personParser = parseJSON



