{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataCmd.Form.TypeToForm where

import Data.Kind (Type)

import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, K1(K1), D1, Constructor (conName), from, U1, Selector (selName), Datatype, datatypeName)
import DataCmd.Generic (Dummy (Dummy))
import DataCmd.Core.Res(Res, (#<), (.#), resNewOR, resNewAN)
import DataCmd.Form

-- | Types whose values can be rendered to Form
class HasF (a :: Type) where aF :: a -> Res Form
instance HasF Bool   where aF x = FPrim (show x) .# "Bool"
instance HasF Int    where aF x = FPrim (show x) .# "Int"
instance HasF Float  where aF x = FPrim (show x) .# "Float"
instance HasF Double where aF x = FPrim (show x) .# "Double"
instance {-# OVERLAPS #-} HasF String where aF s = FPrim s .# "String" -- ^ String list is an exception as literal
instance {-# OVERLAPS #-} HasF a => HasF [a] where aF xs = FΣ (FC "L") . FΠ <$> traverse aF xs #< "List" -- ^ List is product of n fields instead of binary constructed object
instance {-# OVERLAPS #-} (HasF a, HasF b) => HasF (a, b) where aF (x,y) = FΣ (FC "L") . FΠ <$> sequence [aF x,aF y] #< "Tuple"
instance {-# OVERLAPPABLE #-} (Generic a, GF (Rep a), Show a) => HasF a where aF x = genF x #< ("Rendering " <> take 60 (show x) <> "...")

-- | Generic Form renderer
genF :: forall a. (Generic a, GF (Rep a)) => a -> Res Form
genF = gF . from

-- | Typeclass "GF(Generic Form)" whose instances (generic representations) know how to render to Tree
class GF (f :: Type -> Type)  where gF :: f p -> Res Form
instance (GFΣ f, Datatype m) => GF (D1 m f) where gF (M1 x) = resNewOR ("Datatype " <> datatypeName @m Dummy) $ gFΣ False x

-- | "Generic Tree Renderer" logic on generic sum type
class GFΣ (f :: Type -> Type)                   where gFΣ :: Bool -> f p -> Res Form
instance (GFΣ f, GFΣ g)        => GFΣ (f :+: g) where gFΣ _ (L1 x) = gFΣ True x #< "L1"; gFΣ _ (R1 x) = gFΣ True x #< "R1"
instance (GFΠ f, Constructor m) => GFΣ (C1 m f) where gFΣ _ (M1 x) = resNewAN ("Con " <> conName @m Dummy) $ (FΣ (FC $ conName @m Dummy) <$> gFΠ x)

-- | "Generic Tree Renderer" logic on generic product type
class GFΠ (f :: Type -> Type)                        where gFΠ :: f p -> Res FΠ
instance (GFΠ f, GFΠ g) => GFΠ (f :*: g)             where gFΠ (x:*:y) = (<>) <$> (gFΠ x #< "FST") <*> (gFΠ y #< "SND")
instance (HasF a, Selector m) => GFΠ (S1 m (Rec0 a)) where gFΠ (M1 (K1 x)) = (FΠ . pure <$> aF x) #< ("Sel " <> selName @m Dummy)
instance GFΠ U1                                      where gFΠ _ = pure $ FΠ []

