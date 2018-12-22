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
			 PolyKinds #-}
module Subrec.Internal where

import           Data.Kind
import           Data.Proxy
import           Map (Map)
import qualified Map 
import qualified GHC.Generics as GHC
import           Generics.SOP

-- data Subrec [
