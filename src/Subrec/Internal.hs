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
import           Map (Map)
import qualified Map 
import           GHC.TypeLits
import qualified GHC.Generics as GHC
import           Generics.SOP
import qualified Generics.SOP.Type.Metadata as M

data Stuff = forall a . Stuff a

newtype Subrec (ns :: [Type]) r = Subrec (Map String Stuff)

instance (IsProductType r xs, 
          HasDatatypeInfo r,
          ConstructorOf (DatatypeInfoOf r) ~ c,
          ConstructorFieldNamesOf c ~ ns',
          --All (MemberOf ns') ns
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

-- class IsMember (b :: Bool) (x :: Symbol) (ys :: [Symbol])  
--     
-- instance 

--type family MemberOf x (ys :: [Symbol]) :: Bool where
--    MemberOf x '[]       = False
--    MemberOf x (x ': ys) = True
--    MemberOf x (y ': ys) = MemberOf x ys
    


