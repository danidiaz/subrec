{-# LANGUAGE DataKinds, 
             ConstraintKinds,
             PolyKinds,
             RankNTypes,
             TypeFamilies, 
             UndecidableInstances, 
             FlexibleContexts,
             ScopedTypeVariables, 
             TypeApplications, 
             TypeOperators, 
             ExistentialQuantification,
             DeriveFunctor
             #-}
module Subrec.Internal where

import           Data.Proxy
import           Data.Functor.Compose
import           Data.Aeson (FromJSON(..),Object,withObject,(.:))
import           Data.Aeson.Types (Parser)
import           Text (Text)
import qualified Text
import           Set (Set)
import qualified Set 
import           Map (Map)
import qualified Map 
import           GHC.TypeLits
import           Generics.SOP
import           Generics.SOP.NP (liftA_NP,cliftA3_NP,cpure_NP,collapse_NP)
import qualified Generics.SOP.Type.Metadata as M
import           Data.Generics.Product.Fields (HasField')
import           Unsafe.Coerce (unsafeCoerce)

data Stuff = forall a . Show a => Stuff a

instance Show Stuff where
    show (Stuff x) = show x

{-| A record-like type made out of a restricted subset of fields of some
    original record @r@.
    
    The names of the selected fields are given in the type-level list
    @selected@ that parameterizes the @Subrec@.

    The type @r@ should be a plain record type, should be an instance of
    `GHC.Generics.Generic`, and also of `Generics.SOP.Generic` and
    `Generics.SOP.HasDatatypeInfo` from the "generics-sop" package.
-} 
newtype Subrec (selected :: [Symbol]) r = Subrec (Map String Stuff) deriving Show

instance (IsProductType r xs, 
          HasDatatypeInfo r,
          ConstructorOf (DatatypeInfoOf r) ~ c,
          ConstructorNameOf c ~ cn,
          ConstructorFieldNamesOf c ~ ns,
          IsSubset selected ns ~ True,
          KnownSymbol cn,
          All KnownSymbol selected,
          All Show xs,
          All FromJSON xs) => FromJSON (Subrec selected r) where
    parseJSON value = 
        -- Note: this was formerly one big function that didn't use
        -- "subrec" or "fieldNamesProduct". Can splitting 
        -- things hurt compilation time?
        let Compose parsefunc = 
                subrec (Proxy @FromJSON)
                          (\(K fieldName) -> Compose (\o -> o .: Text.pack (fieldName)))
                          (fieldNamesProduct (Proxy @r))
            constructorName :: ConstructorName
            constructorName = 
                symbolVal (Proxy @cn)
         in withObject constructorName parsefunc value

subrec :: forall r xs c ns constraint selected g f.
         (IsProductType r xs, 
          HasDatatypeInfo r,
          ConstructorOf (DatatypeInfoOf r) ~ c,
          ConstructorFieldNamesOf c ~ ns,
          IsSubset selected ns ~ True, -- https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-449764432
          All KnownSymbol selected,
          All constraint xs,
          All Show xs,
          Applicative f) 
       => Proxy constraint
       -> (forall a. constraint a => g a -> f a)
       -> NP g xs -- additional info
       -> f (Subrec selected r) 
subrec _ pure' aliases =  
   let parsers :: NP (g -.-> f) xs 
       parsers = 
           cpure_NP (Proxy @constraint) (Fn pure')
       named :: NP (K (String,f Stuff)) xs
       named = 
           cliftA3_NP (Proxy @Show) 
                      (\(K name) g (Fn trans) -> K (name, Stuff <$> trans g))  
                      (fieldNamesProduct (Proxy @r))
                      aliases
                      parsers
       selected :: Set FieldName
       selected = 
           Set.fromList (demoteFieldNames (Proxy @selected))
       filtered :: Map String (f Stuff)
       filtered = 
           Map.restrictKeys (Map.fromList (collapse_NP named)) selected
    in Subrec <$> sequenceA filtered

fieldNamesProduct :: forall r xs c ns.
                    (IsProductType r xs, 
                     HasDatatypeInfo r,
                     ConstructorOf (DatatypeInfoOf r) ~ c,
                     ConstructorFieldNamesOf c ~ ns)
                  => Proxy r
                  -> NP (K FieldName) xs 
fieldNamesProduct _ = 
    case constructorInfo (datatypeInfo (Proxy @r)) of
        Record _ fields :* Nil -> 
            liftA_NP (\(FieldInfo name) -> K name) fields
        _ -> error "Not a record. Never happens."

{-| Extract a field value from a @Subrec@. 
    
    We don't use the lens provided by @HasField'@ at all, we only use the
    constraint to enforce that the field exists in the original record, and for
    the functional dependency to know what the return type should be.
-}
subGetField :: forall field selected r v. (KnownSymbol field, IsMember field selected ~ True, HasField' field r v) 
            => Proxy field 
            -> Subrec selected r -> v
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
    IsSubset (x ': xs) ys = LogicalAnd (IsMember x ys) (IsSubset xs ys)  

type family LogicalAnd (b::Bool) (b'::Bool) :: Bool where
    LogicalAnd True True = True
    LogicalAnd _    _    = False

demoteFieldNames :: forall ns. (All KnownSymbol ns) => Proxy ns -> [FieldName] 
demoteFieldNames p = unK $ cpara_SList (Proxy @KnownSymbol) (K []) step `sameTag` p
  where
    step :: forall (y :: Symbol) (ys :: [Symbol]). (KnownSymbol y, All KnownSymbol ys) 
         => K [FieldName] ys 
         -> K [FieldName] (y ': ys)
    step (K foo) = K (symbolVal (Proxy @y) : foo)
    sameTag :: forall x y a . x a -> y a -> x a
    sameTag = const

