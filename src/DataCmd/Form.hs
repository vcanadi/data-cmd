{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataCmd.Form where

import Data.Kind (Type)

import GHC.Generics ((:+:) (..), Generic (Rep), C1, S1, Rec0, D1, Constructor (conName), U1, (:*:), Datatype, datatypeName)
import DataCmd.Generic (Dummy (Dummy))
import DataCmd.Core.Res(Res, (#<), (.#), resNewAN)
import Data.Proxy (Proxy (Proxy))
import Control.Applicative ((<|>))


-- Value level
--
-- | Tree product constructor
newtype FC = FC String deriving (Show, Eq, Generic)


-- | Simplified generic value representation as a value-level tree
data Form = FΣ FC FΠ deriving (Eq, Generic)
newtype FΠ = FΠ [Form] deriving (Show, Eq, Generic)


pattern FPrim :: String -> Form
pattern FPrim c = FΣ (FC c) (FΠ [])

pattern (:..) :: String -> [Form] -> Form
pattern (:..) c ts = FΣ (FC c) (FΠ ts)

-- | More concise Show instance that looks like pattern synonym construction
instance Show Form where
  show (FPrim c) = "FPrim " <> c
  show (c :.. ps) = show c <> " :.. " <> show ps

instance Semigroup FΠ where FΠ ps0 <> FΠ ps1 = FΠ $ ps0 <> ps1

-- Type level

-- | Tree product constructor
newtype FTyC = FTyC String deriving (Show, Eq)

-- | Simplified generic type representation as a value-level tree
newtype FTy = FTyΣ [(FTyC, FTyΠ)] deriving (Show, Eq)
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
instance (GFTyΣ f, Datatype m) => GFTy (D1 m f) where gFTy _ = FTyΣ <$> resNewAN ("Datatype " <> datatypeName @m Dummy) (gFTyΣ False (Proxy @f))

-- | "Generic Tree Renderer" logic on generic sum type
class GFTyΣ (f :: Type -> Type)                      where gFTyΣ :: Bool -> Proxy f -> Res [(FTyC, FTyΠ)]
instance (GFTyΣ f, GFTyΣ g)       => GFTyΣ (f :+: g) where gFTyΣ _ _ = (gFTyΣ True (Proxy @f) #< "L1") <|> (gFTyΣ True (Proxy @g) #< "R1")
instance (GFTyΠ f, Constructor m) => GFTyΣ (C1 m f)  where gFTyΣ _ _ = pure . (FTyC $ conName @m Dummy,) <$> resNewAN ("Con " <> conName @m Dummy) (gFTyΠ (Proxy @f))

-- | "Generic Tree Renderer" logic on generic product type
class GFTyΠ (f :: Type -> Type)                where gFTyΠ :: Proxy f -> Res FTyΠ
instance (GFTyΠ f, GFTyΠ g) => GFTyΠ (f :*: g) where gFTyΠ _ = (<>) <$> (gFTyΠ (Proxy @f) #< "FST") <*> (gFTyΠ (Proxy @g) #< "SND")
instance HasFTy a => GFTyΠ (S1 m (Rec0 a))     where gFTyΠ _ = FTyΠ . pure <$> aFTy (Proxy @a) #< "Type"
instance GFTyΠ U1                              where gFTyΠ _ = pure $ FTyΠ []
