
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DataCmd.Tree where

import Data.Kind (Type)

import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, K1(K1), D1, Constructor (conName), from, U1)
import Data.Bool (bool)
import DataCmd.Generic (Dummy (Dummy))
import DataCmd.Util (Res, (##), (.#))

data X2 = X20 | X21 Int | X22 Int Float deriving (Show, Eq, Generic)

-- -- | Tree product constructor
-- newtype TC = TC String deriving (Show, Eq)

-- -- | Simplified generic type representation as a value-level tree
-- data T = TPrim String | T TΣ deriving (Show, Eq)
-- newtype TΣ = TΣ [(TC,TΠ)] deriving (Show, Eq)
-- newtype TΠ = TΠ [T] deriving (Show, Eq)

-- instance Semigroup TΣ where TΣ ss0 <> TΣ ss1 = TΣ $ ss0 <> ss1
-- instance Semigroup TΠ where TΠ ps0 <> TΠ ps1 = TΠ $ ps0 <> ps1

-- -- | Types that can be rendered to T
-- class HasTR (a :: Type) where aTR :: a -> Res T
-- instance HasTR Bool   where aTR x = TPrim (show x) .# "Bool"
-- instance HasTR Int    where aTR x = TPrim (show x) .# "Int"
-- instance HasTR Float  where aTR x = TPrim (show x) .# "Float"
-- instance HasTR Double where aTR x = TPrim (show x) .# "Double"
-- instance {-# OVERLAPS #-} HasTR String where aTR s = TPrim s .# "String" -- ^ String list is an exception as literal
-- instance {-# OVERLAPS #-} HasTR a => HasTR [a] where aTR xs = T (TΣ [(TC "L", TΠ $ aTR <$> xs)]) ## "List" -- ^ List is product of n fields instead of binary constructed object
-- instance {-# OVERLAPPABLE #-} (Generic a, GTR (Rep a)) => HasTR a where aTR = genTR

-- -- | Generic Tree Renderer
-- genTR :: forall a. (Generic a, GTR (Rep a)) => a -> Res T
-- genTR = gTR . from

-- -- | Typeclass "GTR(Generic Tree Renderer)" whose instances (generic representations) know how to render to Tree
-- class GTR (f :: Type -> Type)   where gTR :: f p -> Res T
-- instance GTRΣ f => GTR (D1 m f) where gTR (M1 x) = T $ gTRΣ False x

-- -- | "Generic Tree Renderer" logic on generic sum type
-- class GTRΣ (f :: Type -> Type)                     where gTRΣ :: Bool -> f p -> Res TΣ
-- instance (GTRΣ f, GTRΣ g)        => GTRΣ (f :+: g) where gTRΣ _ (L1 x) = gTRΣ True x; gTRΣ _ (R1 x) = gTRΣ True x
-- instance (GTRΠ f, Constructor m) => GTRΣ (C1 m f)  where gTRΣ h' (M1 x) = (\y -> TΣ (bool [] [(TC $ conName @m Dummy, y)] h')) $ gTRΠ x

-- -- | "Generic Tree Renderer" logic on generic product type
-- class GTRΠ (f :: Type -> Type)              where gTRΠ :: f p -> Res TΠ
-- instance (GTRΠ f, GTRΠ g) => GTRΠ (f :*: g) where gTRΠ (x:*:y) = gTRΠ x <> gTRΠ y
-- instance HasTR a => GTRΠ (S1 m (Rec0 a))    where gTRΠ (M1 (K1 x)) = TΠ [aTR x]
-- instance GTRΠ U1                            where gTRΠ _ = TΠ []
