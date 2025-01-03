{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}

module DataCmd.Generic where

import Data.Kind (Type)
import Data.Proxy
import GHC.Generics (M1 (..), (:+:) (..), Generic (Rep), Meta (..), C1, S1, Rec0, D1, Constructor (conName), Datatype (datatypeName), (:*:), U1, K1 )
import Data.Typeable (typeRep)

-- | Max Width (max number of fields of any product)
class                    MW (f :: Type -> Type) where mW :: Proxy f -> Int
instance (MW f, MW g) => MW (f :+: g)           where mW _ = max (mW (Proxy @f)) (mW (Proxy @g))
instance (MW f, MW g) => MW (f :*: g)           where mW _ = mW (Proxy @f) + mW (Proxy @g)
instance                 MW (K1 i c)            where mW _ = 1
instance MW f         => MW (M1 i t f)          where mW _ = mW (Proxy @f)
instance                 MW U1                  where mW _ = 0

-- | Height (number of constructors in a sum type)
class H (f :: Type -> Type)        where h :: Proxy f -> Int
instance (H f, H g) => H (f :+: g) where h _ = h (Proxy @f) + h (Proxy @g)
instance H (f :*: g)               where h _ = 1
instance H (C1 i c)                where h _ = 1
instance H (S1 m r)                where h _ = 1
instance H U1                      where h _ = 1

-- | Name of the type
class TypNm (a :: Type) where typNm :: Proxy a -> String
instance TypNm Bool   where typNm _ = show $ typeRep (Proxy @Bool)
instance TypNm Int    where typNm _ = show $ typeRep (Proxy @Int)
instance TypNm Float  where typNm _ = show $ typeRep (Proxy @Float)
instance TypNm Double where typNm _ = show $ typeRep (Proxy @Double)
instance TypNm String where typNm _ = show $ typeRep (Proxy @String)
instance {-# OVERLAPS #-} TypNm a => TypNm [a] where typNm _ = "[" <> typNm (Proxy @a) <> "]"
instance {-# OVERLAPPABLE #-} (GTypNm (Rep a)) => TypNm a where typNm _ = gTypNm (Proxy @(Rep a))


-- | Show type (more than just name)
class TypNm a => SH (a :: Type) where sh :: [String] -> Int -> Proxy a -> String
instance SH Bool   where sh _ d _ = tab d <> typNm (Proxy @Bool)
instance SH Int    where sh _ d _ = tab d <> typNm (Proxy @Int)
instance SH Float  where sh _ d _ = tab d <> typNm (Proxy @Float)
instance SH Double where sh _ d _ = tab d <> typNm (Proxy @Double)
instance SH String where sh _ d _ = tab d <> typNm (Proxy @String)
instance {-# OVERLAPS #-} SH a => SH [a] where sh us d _ = tab d <> "[" <> sh us d (Proxy @a) <> "]"
instance {-# OVERLAPPABLE #-} (GSH (Rep a)) => SH a where sh us d _ = gSH us d (Proxy @(Rep a))

-- | Generic representation variant of SH
-- Typeclass "GSH(Generic Show)" whose instances (generic representations) can be shown
-- List of TypNms already shown types is passed down in order to avoid infinite recursion
class GTypNm f => GSH (f :: Type -> Type)     where gSH :: [String] -> Int -> Proxy f -> String
instance (GSHΣ f, Datatype m) => GSH (D1 m f) where gSH us d _ = gSHΣ (gTypNm (Proxy @(D1 m f)):("["<>gTypNm (Proxy @(D1 m f))<>"]"):us) d (Proxy @f)

tab :: Int -> String
tab d = "\n" <> concat (replicate d "   ")

-- | "Generic Show" logic on generic sum type
class GSHΣ (f :: Type -> Type)              where gSHΣ :: [String] -> Int -> Proxy f -> String
instance (GSHΣ f, GSHΣ g) => GSHΣ (f :+: g) where gSHΣ us d _ = tab d <> gSHΣ us (succ d) (Proxy @f)
                                                             <> tab d <> "+" <> show d <> "+"  <> gSHΣ us (succ d) (Proxy @g)

instance (GSHπ f, Constructor m) => GSHΣ (C1 m f)  where gSHΣ us d _ = tab d <> conName @m Dummy  <> gSHπ us d (Proxy @f)

-- Constraints aliases for brevity
--
-- | "Generic Parser" logic on generic product type
class GSHπ (f :: Type -> Type)              where gSHπ :: [String] -> Int -> Proxy f -> String
instance (GSHπ f, GSHπ g) => GSHπ (f :*: g) where gSHπ us d _ = tab d <> gSHπ us (succ d) (Proxy @f)
                                                                   <> tab d <> "*" <> show d <> "*"  <> gSHπ us (succ d) (Proxy @g)
instance SH a => GSHπ (S1 m (Rec0 a))       where gSHπ us d _ = if typNm (Proxy @a) `elem` us
                                                                     then tab d <> "{" <> typNm (Proxy @a) <> "}"
                                                                     else sh (typNm (Proxy @a):us) d (Proxy @a)
instance GSHπ U1                            where gSHπ _ _ _ = ""

data Dummy (t :: Meta) (c :: Type -> Type) f = Dummy

-- | Generic representation variant of TypNm
class GTypNm (f :: Type -> Type)         where gTypNm :: Proxy f -> String
instance (Datatype m) => GTypNm (D1 m f) where gTypNm _ = datatypeName @m Dummy
