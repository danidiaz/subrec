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
import           Set (Set)
import qualified Set 
import           Map (Map)
import qualified Map 
import           GHC.TypeLits
--import qualified GHC.Generics as GHC
import           Generics.SOP
import           Generics.SOP.NP (liftA_NP,cliftA2_NP,cpure_NP,collapse_NP)
import qualified Generics.SOP.Type.Metadata as M
import           Data.Generics.Product.Fields
import           Unsafe.Coerce

data Stuff = forall a . Show a => Stuff a

instance Show Stuff where
    show (Stuff x) = show x

newtype Subrec (ns :: [Symbol]) r = Subrec (Map String Stuff) deriving Show

instance (IsProductType r xs, 
          HasDatatypeInfo r,
          ConstructorOf (DatatypeInfoOf r) ~ c,
          ConstructorNameOf c ~ cn,
          KnownSymbol cn,
          ConstructorFieldNamesOf c ~ ns',
          IsSubset ns ns' ~ True,
          DemotableFieldNameList ns,
          All Show xs,
          All FromJSON xs) => FromJSON (Subrec ns r) where
    parseJSON value = 
        let fieldNames :: NP (K FieldName) xs 
            fieldNames = 
                case constructorInfo (datatypeInfo (Proxy @r)) of
                    Record _ fields :* Nil -> 
                        liftA_NP (\(FieldInfo name) -> K name) fields
                    _ -> error "Not a record. Never happens."
            restriction :: Set FieldName
            restriction = 
                Set.fromList (demoteFieldNameList (Proxy @ns))
            parsers :: NP Parser2 xs 
            parsers = 
                cpure_NP (Proxy @FromJSON) 
                         (Parser2 (\fieldName o -> o .: Text.pack (fieldName)))
            items :: NP (K (String, Object -> Parser Stuff)) xs
            items = 
                cliftA2_NP (Proxy @Show) 
                           (\(K name) (Parser2 f) -> K (name,fmap (fmap Stuff) (f name)))  
                           fieldNames
                           parsers
            filteredMap :: Map String (Object -> Parser Stuff)
            filteredMap = 
                Map.restrictKeys (Map.fromList (collapse_NP items)) restriction
            traversedMap :: Object -> Parser (Map String Stuff)
            Parser1 traversedMap = 
                traverse Parser1 filteredMap
            constructorName :: ConstructorName
            constructorName = 
                symbolVal (Proxy @cn)
         in Subrec <$> withObject constructorName traversedMap value

subGetField :: forall field ns r v. (KnownSymbol field, IsMember field ns ~ True, HasField' field r v) => Proxy field -> Subrec ns r -> v
subGetField _ (Subrec m ) = 
     case Map.lookup (symbolVal (Proxy @field)) m of
         Nothing -> error "Field not found. Never happens."
         Just (Stuff stuff) -> unsafeCoerce stuff

type family ConstructorNameOf (a :: M.ConstructorInfo) :: Symbol where
    ConstructorNameOf ('M.Record constructorName fields) = constructorName

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

-- TODO
-- Is there a way of not having to define DemotableFieldNameList and demote the
-- symbol list using SListI ?
