{-# LANGUAGE DataKinds, 
             TypeFamilies, 
             FlexibleContexts, 
             UndecidableInstances, 
             DeriveFunctor, 
             DeriveFoldable, 
             DeriveTraversable, 
             DeriveGeneric, 
             KindSignatures, 
             ConstraintKinds, 
             TypeOperators, 
             TypeApplications, 
             ScopedTypeVariables, 
             ViewPatterns, 
             FlexibleInstances, 
             TypeOperators, 
             PolyKinds,
             ExistentialQuantification,
             MultiParamTypeClasses,
             AllowAmbiguousTypes
             #-}
module Subrec.Internal where

import           Data.Kind
import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Type.Equality (type (==))
import           Text (Text)
import qualified Text
import           Map (Map)
import qualified Map 
import           GHC.TypeLits
--import qualified GHC.Generics as GHC
import           Generics.SOP
import           Generics.SOP.NP (liftA_NP,cpure_NP)
import qualified Generics.SOP.Type.Metadata as M

data Stuff = forall a . Stuff a

newtype Subrec (ns :: [Symbol]) r = Subrec (Map String Stuff)

instance (IsProductType r xs, 
          HasDatatypeInfo r,
          ConstructorOf (DatatypeInfoOf r) ~ c,
          ConstructorFieldNamesOf c ~ ns',
          IsSubset ns ns' ~ True,
          DemotableFieldNameList ns,
          All FromJSON xs) => FromJSON (Subrec ns r) where
    parseJSON v = 
        let ns' :: NP (K FieldName) xs = 
                case constructorInfo (datatypeInfo (Proxy @r)) of
                    Record _ fields :* Nil -> 
                        liftA_NP (\(FieldInfo name) -> K name) fields
                    _ -> error "not going to happen"
            ns = demoteFieldNameList (Proxy @ns)
            parsers :: NP Parser2 xs = 
                cpure_NP (Proxy @FromJSON) 
                         (Parser2 (\fieldName o -> o .: Text.pack (fieldName)))
         in undefined

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

newtype Parser1 a = Parser1 { parseJSON1 :: Object -> Data.Aeson.Types.Parser a } deriving Functor

instance Applicative Parser1 where
    pure x = Parser1 (pure (pure x))
    Parser1 pa <*> Parser1 pb = Parser1 $ \v -> pa v <*> pb v 

newtype Parser2 a = Parser2 { parseJSON2 :: FieldName -> Object -> Data.Aeson.Types.Parser a } 

class DemotableFieldNameList (xs :: [Symbol]) where
    demoteFieldNameList :: Proxy xs -> [FieldName] 

instance DemotableFieldNameList '[] where
    demoteFieldNameList _ = []

instance (KnownSymbol x, DemotableFieldNameList xs) => DemotableFieldNameList (x ': xs) where
    demoteFieldNameList _ = symbolVal (Proxy @x) : demoteFieldNameList (Proxy @xs)
