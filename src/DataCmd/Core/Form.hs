{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module DataCmd.Core.Form where

import Data.Kind (Type)

import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, K1(K1), D1, Constructor (conName), from, U1)
import DataCmd.Generic (Dummy (Dummy))
import DataCmd.Core.Res(Res, (#<), (.#))
import Data.Proxy (Proxy (Proxy))
import Control.Applicative ((<|>))


-- Value level
--
-- | Tree product constructor
newtype FC = FC String deriving (Show, Eq)

-- | Simplified generic value representation as a value-level tree
data F = FΣ FC FΠ deriving (Show, Eq)
newtype FΠ = FΠ [F] deriving (Show, Eq)

pattern FPrim :: String -> F
pattern FPrim c = FΣ (FC c) (FΠ [])

pattern (:..) :: String -> [F] -> F
pattern (:..) c ts = FΣ (FC c) (FΠ ts)

instance Semigroup FΠ where FΠ ps0 <> FΠ ps1 = FΠ $ ps0 <> ps1

-- | Types whose values can be rendered to F
class HasF (a :: Type) where aF :: a -> Res F
instance HasF Bool   where aF x = FPrim (show x) .# "Bool"
instance HasF Int    where aF x = FPrim (show x) .# "Int"
instance HasF Float  where aF x = FPrim (show x) .# "Float"
instance HasF Double where aF x = FPrim (show x) .# "Double"
instance {-# OVERLAPS #-} HasF String where aF s = FPrim s .# "String" -- ^ String list is an exception as literal
instance {-# OVERLAPS #-} HasF a => HasF [a] where aF xs = FΣ (FC "L") . FΠ <$> traverse aF xs #< "List" -- ^ List is product of n fields instead of binary constructed object
instance {-# OVERLAPPABLE #-} (Generic a, GF (Rep a)) => HasF a where aF x = genF x #< "Gen"

-- | Generic F renderer
genF :: forall a. (Generic a, GF (Rep a)) => a -> Res F
genF = gF . from

-- | Typeclass "GF(Generic F)" whose instances (generic representations) know how to render to Tree
class GF (f :: Type -> Type)  where gF :: f p -> Res F
instance GFΣ f => GF (D1 m f) where gF (M1 x) = gFΣ False x #< "F"

-- | "Generic Tree Renderer" logic on generic sum type
class GFΣ (f :: Type -> Type)                   where gFΣ :: Bool -> f p -> Res F
instance (GFΣ f, GFΣ g)        => GFΣ (f :+: g) where gFΣ _ (L1 x) = gFΣ True x #< "LHS"; gFΣ _ (R1 x) = gFΣ True x #< "RHS"
instance (GFΠ f, Constructor m) => GFΣ (C1 m f) where gFΣ _ (M1 x) = FΣ (FC $ conName @m Dummy) <$> gFΠ x #< "C"

-- | "Generic Tree Renderer" logic on generic product type
class GFΠ (f :: Type -> Type)            where gFΠ :: f p -> Res FΠ
instance (GFΠ f, GFΠ g) => GFΠ (f :*: g) where gFΠ (x:*:y) = (gFΠ x #< "FST") <> (gFΠ y #< "SND")
instance HasF a => GFΠ (S1 m (Rec0 a))   where gFΠ (M1 (K1 x)) = FΠ . pure <$> aF x #< "Type"
instance GFΠ U1                          where gFΠ _ = pure $ FΠ []





-- Type level

-- | Tree product constructor
newtype FTyC = FTyC String deriving (Show, Eq)

-- | Simplified generic type representation as a value-level tree
data FTy = FTyΣ [(FTyC, FTyΠ)] deriving (Show, Eq)
newtype FTyΠ = FTyΠ [FTy] deriving (Show, Eq)

instance Semigroup FTyΠ where FTyΠ ps0 <> FTyΠ ps1 = FTyΠ $ ps0 <> ps1

pattern FTyPrim :: String -> FTy
pattern FTyPrim c = FTyΣ [(FTyC c, FTyΠ [])]

pattern (:.:) :: String -> [FTy] -> FTy
pattern (:.:) c ts = FTyΣ [(FTyC c, FTyΠ ts)]

-- | Types that can be rendered to FTy
class HasFTy (a :: Type) where aFTy :: Proxy a -> Res FTy
instance HasFTy Bool   where aFTy _ = FTyPrim "Bool" .# "Bool"
instance HasFTy Int    where aFTy _ = FTyPrim "Int" .# "Int"
instance HasFTy Float  where aFTy _ = FTyPrim "Float" .# "Float"
instance HasFTy Double where aFTy _ = FTyPrim "Double" .# "Double"
instance {-# OVERLAPS #-} HasFTy String where aFTy _ = FTyPrim "String" .# "String" -- ^ String list is an exception as literal
instance {-# OVERLAPS #-} HasFTy a => HasFTy [a] where aFTy _ = FTyΣ . pure . (FTyC "L",) . FTyΠ . pure <$> aFTy (Proxy @a) #< "List" -- ^ List is product of n fields instead of binary constructed object
instance {-# OVERLAPPABLE #-} GFTy (Rep a) => HasFTy a where aFTy _ = genFTy (Proxy @a) #< "Generic"

-- | Generic F renderer
genFTy :: forall a. GFTy (Rep a) => Proxy a -> Res FTy
genFTy _ = gFTy (Proxy @(Rep a))

-- | Typeclass "GFTy(Generic FTy)" whose instances (generic representations) know how to render to Tree
class GFTy (f :: Type -> Type)  where gFTy :: Proxy f -> Res FTy
instance GFTyΣ f => GFTy (D1 m f) where gFTy _ = FTyΣ <$> gFTyΣ False (Proxy @f) #< "Form"

-- | "Generic Tree Renderer" logic on generic sum type
class GFTyΣ (f :: Type -> Type)                      where gFTyΣ :: Bool -> Proxy f -> Res [(FTyC, FTyΠ)]
instance (GFTyΣ f, GFTyΣ g)       => GFTyΣ (f :+: g) where gFTyΣ _ _ = (gFTyΣ True (Proxy @f) #< "L1") <> (gFTyΣ True (Proxy @g) #< "R1")
instance (GFTyΠ f, Constructor m) => GFTyΣ (C1 m f)  where gFTyΣ _ _ = pure . (FTyC $ conName @m Dummy,) <$> gFTyΠ (Proxy @f) #< ("Con " <> conName @m Dummy )

-- | "Generic Tree Renderer" logic on generic product type
class GFTyΠ (f :: Type -> Type)                where gFTyΠ :: Proxy f -> Res FTyΠ
instance (GFTyΠ f, GFTyΠ g) => GFTyΠ (f :*: g) where gFTyΠ _ = (gFTyΠ (Proxy @f) #< "FST") <> (gFTyΠ (Proxy @g) #< "SND")
instance HasFTy a => GFTyΠ (S1 m (Rec0 a))       where gFTyΠ _ = FTyΠ . pure <$> aFTy (Proxy @a) #< "Type"
instance GFTyΠ U1                              where gFTyΠ _ = pure $ FTyΠ []
