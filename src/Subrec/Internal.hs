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
          All (IsMember True ns') ns,
          All FromJSON xs) => FromJSON (Subrec ns r) where
    parseJSON _ = undefined

--subParser :: Proxy (ns :: [Type]) -> 
--subParser = undefined

type family ConstructorOf (a :: M.DatatypeInfo) :: M.ConstructorInfo where
    ConstructorOf ('M.ADT moduleName datatypeName '[constructor]) = constructor

type family ConstructorFieldNamesOf (a :: M.ConstructorInfo) :: [Symbol] where
    ConstructorFieldNamesOf ('M.Record constructorName fields) = FieldNamesOf fields

type family FieldNamesOf (a :: [M.FieldInfo]) :: [Symbol] where
    FieldNamesOf '[] = '[]
    FieldNamesOf (('M.FieldInfo n) ': xs) = n ': FieldNamesOf xs

class IsMember (b :: Bool) (ys :: [Symbol]) (x :: Symbol)   
    
instance IsMember True (x ': xs) x

instance ((x == y) ~ False, IsMember True xs x) => IsMember True (y ': xs) x 

--type family MemberOf x (ys :: [Symbol]) :: Bool where
--    MemberOf x '[]       = False
--    MemberOf x (x ': ys) = True
--    MemberOf x (y ': ys) = MemberOf x ys
    
data Person = Person { name :: String, age :: Int } deriving (Show,GHC.Generic)
instance Generic Person
instance HasDatatypeInfo Person

personParser :: Value -> Parser (Subrec '["name"] Person)
personParser = parseJSON



