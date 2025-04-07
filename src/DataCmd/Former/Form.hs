

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module DataCmd.Former.Form where

import Data.Kind (Type)

import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, K1(K1), D1, Constructor (conName), from, U1)
import DataCmd.Generic (Dummy (Dummy))
import DataCmd.Core.Res(Res, (##), (.#))

data X2 = X20 | X21 Int | X22 Int Float deriving (Show, Eq, Generic)

-- | Tree product constructor
newtype FC = FC String deriving (Show, Eq)

-- | Simplified generic type representation as a value-level tree
data F = FPrim String | F FΣ deriving (Show, Eq)
data FΣ = FΣ FC FΠ deriving (Show, Eq)
newtype FΠ = FΠ [F] deriving (Show, Eq)

pattern (:..) :: String -> [F] -> F
pattern (:..) c ts = F (FΣ (FC c) (FΠ ts))

instance Semigroup FΠ where FΠ ps0 <> FΠ ps1 = FΠ $ ps0 <> ps1

-- | Types that can be rendered to F
class HasF (a :: Type) where aF :: a -> Res F
instance HasF Bool   where aF x = FPrim (show x) .# "Bool"
instance HasF Int    where aF x = FPrim (show x) .# "Int"
instance HasF Float  where aF x = FPrim (show x) .# "Float"
instance HasF Double where aF x = FPrim (show x) .# "Double"
instance {-# OVERLAPS #-} HasF String where aF s = FPrim s .# "String" -- ^ String list is an exception as literal
instance {-# OVERLAPS #-} HasF a => HasF [a] where aF xs = F . FΣ (FC "L") . FΠ <$> traverse aF xs ## "List" -- ^ List is product of n fields instead of binary constructed object
instance {-# OVERLAPPABLE #-} (Generic a, GF (Rep a)) => HasF a where aF x = genF x ## "Gen"

-- | Generic F renderer
genF :: forall a. (Generic a, GF (Rep a)) => a -> Res F
genF = gF . from

-- | Typeclass "GF(Generic F)" whose instances (generic representations) know how to render to Tree
class GF (f :: Type -> Type)  where gF :: f p -> Res F
instance GFΣ f => GF (D1 m f) where gF (M1 x) = F <$> gFΣ False x ## "F"

-- | "Generic Tree Renderer" logic on generic sum type
class GFΣ (f :: Type -> Type)                   where gFΣ :: Bool -> f p -> Res FΣ
instance (GFΣ f, GFΣ g)        => GFΣ (f :+: g) where gFΣ _ (L1 x) = gFΣ True x ## "LHS"; gFΣ _ (R1 x) = gFΣ True x ## "RHS"
instance (GFΠ f, Constructor m) => GFΣ (C1 m f) where gFΣ _ (M1 x) = FΣ (FC $ conName @m Dummy) <$> gFΠ x ## "C"

-- | "Generic Tree Renderer" logic on generic product type
class GFΠ (f :: Type -> Type)            where gFΠ :: f p -> Res FΠ
instance (GFΠ f, GFΠ g) => GFΠ (f :*: g) where gFΠ (x:*:y) = (gFΠ x ## "FST") <> (gFΠ y ## "SND")
instance HasF a => GFΠ (S1 m (Rec0 a))   where gFΠ (M1 (K1 x)) = FΠ . pure <$> aF x ## "Type"
instance GFΠ U1                          where gFΠ _ = pure $ FΠ []

