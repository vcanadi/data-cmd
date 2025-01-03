{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DataCmd.Act.Parser where

import Data.Kind (Type)

import Data.Proxy
import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), Meta (..), C1, S1, Rec0, U1(U1), K1(K1), D1, to, Constructor (conName), Datatype, Selector (..), from)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Text.Read (readEither)
import Data.Char (toLower)
import Control.Monad ((<=<))
import Data.Bool (bool)
import DataCmd.Lexer
import DataCmd.Generic

-- Parser
--
-- | Tree parser,
type TP a = Tree -> Res a

-- | Tree parser that matchines leaf and reads content
readTP :: forall a. (TypNm a, Read a) => [String] -> TP a
readTP adr (Leaf s) = parseEither adr s
readTP adr t = parseErr (typNm (Proxy @a):adr) $ ", expecting Leaf, got: " <> showT t


class TypNm a => HasTP (a :: Type) where aTP :: [String] -> Proxy a -> TP a
instance HasTP Bool   where aTP adr _ = readTP adr
instance HasTP Int    where aTP adr _ = readTP adr
instance HasTP Float  where aTP adr _ = readTP adr
instance HasTP Double where aTP adr _ = readTP adr
instance {-# OVERLAPS #-} HasTP String where aTP adr _ = readTP adr
instance {-# OVERLAPS #-} HasTP a => HasTP [a] where aTP adr _ = \case (Node cs) -> traverse (aTP adr (Proxy @a)) cs
                                                                       _         -> Left [["Error parsing list"]]
instance {-# OVERLAPPABLE #-} (Generic a, GTP (Rep a), GTypNm (Rep a)) => HasTP a where aTP adr _ = genTP adr

-- | Generic Tree Parser
genTP :: forall a. (Generic a, GTP (Rep a)) => [String] -> TP a
genTP adr = fmap (to @a) . gTP adr (Proxy @(Rep a))

-- Parser

-- | Typeclass "GTP(Generic Tree Parser)" whose instances (generic representations) know how to generate tree parser
class GTP (f :: Type -> Type)                            where gTP :: [String] -> Proxy f -> TP (f p)
instance (GTPΣ f, H f, MW f, Datatype m) => GTP (D1 m f) where gTP adr _ = fmap M1 . gTPΣ (adr<>[gTypNm (Proxy @(D1 m f))]) (h $ Proxy @f) (mW $ Proxy @f) (Proxy @f)

-- Constraints aliases for brevity
--
type GTPΣ_C1 f (m :: Meta) = ( GTPπ f, Typeable f, Constructor m, MW f)


-- | "Generic Tree Parser" logic on generic sum type
class GTPΣ (f :: Type -> Type)                     where gTPΣ :: [String] -> Int -> Int -> Proxy f -> TP (f p)
instance (GTPΣ f, GTPΣ g)        => GTPΣ (f :+: g) where gTPΣ adr h w _ t = (L1 <$> gTPΣ adr h w (Proxy @f) t) `resOR` (R1 <$> gTPΣ adr h w (Proxy @g) t)
instance (GTPΣ_C1 f (m :: Meta)) => GTPΣ (C1 m f)  where gTPΣ adr h w _ t = if h == 1
                                                                            then M1 <$> gTPπ adr (mW $ Proxy @f) (Proxy @f) t -- ^ Ignore constructor when type has a single constructor
                                                                            else
                                                                            case t of
                                                                              Node (Leaf s:ts) | conEq s -> M1 <$> gTPπ adr (mW $ Proxy @f) (Proxy @f) (Node ts)
                                                                              -- | Allow replacing double nest with single in case of enum (w==0)
                                                                              (Leaf s)         | conEq s && w == 0 -> M1 <$> gTPπ adr (mW $ Proxy @f) (Proxy @f) (Node [])
                                                                                               | otherwise -> parseErr adrWithCon $
                                                                                                              showT t <> ", left leaf:(" <> s <> ") does not match expected constructor"
                                                                              _ -> parseErr adrWithCon $ showT t <> ", expecting constructor name in left leaf"
                                                                              where
                                                                                adrWithCon = adr <> [conName @m Dummy]
                                                                                conEq s = fmap toLower (conName @m Dummy) == fmap toLower s

-- Constraints aliases for brevity
--
type GTPπ_Prod f g = (GTPπ f, GTPπ g, MW f)
type GTPπ_S1 a (m :: Meta) = (HasTP a, TypNm a, Selector m)

-- | "Generic Tree Parser" logic on generic product type
class GTPπ (f :: Type -> Type)                         where gTPπ :: [String] -> Int -> Proxy f -> TP (f p)
instance GTPπ_Prod f g => GTPπ (f :*: g)               where gTPπ adr w _ = \case t@(Leaf _) -> parseErr (":*:":adr) $ showT t <> ", expecting Leaf"
                                                                                  (Node ts) ->
                                                                                    (:*:) <$> gTPπ adr w (Proxy @f) (Node $ take (mW (Proxy @f)) ts)
                                                                                          <*> gTPπ adr w (Proxy @g) (Node $ drop (mW (Proxy @f)) ts)
instance GTPπ_S1 a (m :: Meta) => GTPπ (S1 m (Rec0 a)) where gTPπ adr w _ = \case (Node [t])          -> fmap (M1 . K1) (aTP (adr<>[selName @m Dummy]) (Proxy @a) t)
                                                                                  t@(Leaf _) | w == 1 -> fmap (M1 . K1) (aTP (adr<>[selName @m Dummy]) (Proxy @a) t)
                                                                                  t                   -> parseErr (adr<>[selName @m Dummy]) $ showT t <> ", expecting singleton Node"
instance GTPπ U1                                       where gTPπ _ _ _ = const $ Right U1

parseErr :: [String] -> String -> Res a
parseErr adr msg = Left [["TP err; type: " <> intercalate "." adr <> ", msg: " <> msg]]

actTP :: TP Act
actTP = aTP [] (Proxy @Act)
parseAct :: String -> Res Act
parseAct = actTP <=< tNormalLex

parseEither :: Read a => [String] -> String -> Res a
parseEither adr w = case readEither w of
  Left msg -> parseErr (adr<>[msg]) w
  Right x -> Right x


-- Renderer
--
-- | Tree parser that matchines leaf and reads content

class HasTR (a :: Type) where aTR :: a -> Tree
instance HasTR Bool   where aTR x = Leaf $ show x
instance HasTR Int    where aTR x = Leaf $ show x
instance HasTR Float  where aTR x = Leaf $ show x
instance HasTR Double where aTR x = Leaf $ show x
instance {-# OVERLAPS #-} HasTR String where aTR = Leaf
instance {-# OVERLAPS #-} HasTR a => HasTR [a] where aTR xs = Node $ aTR <$> xs
instance {-# OVERLAPPABLE #-} (Generic a, GTR (Rep a)) => HasTR a where aTR = genTR

-- | Generic Tree Renderer
genTR :: forall a. (Generic a, GTR (Rep a)) => a -> Tree
genTR = gTR . from

-- | Typeclass "GTR(Generic Tree Renderer)" whose instances (generic representations) know how to render to Tree
class GTR (f :: Type -> Type)   where gTR :: f p -> Tree
instance GTRΣ f => GTR (D1 m f) where gTR (M1 x) = gTRΣ False x

-- | "Generic Tree Renderer" logic on generic sum type
class GTRΣ (f :: Type -> Type)                     where gTRΣ :: Bool -> f p -> Tree
instance (GTRΣ f, GTRΣ g)        => GTRΣ (f :+: g) where gTRΣ _ (L1 x) = gTRΣ True x; gTRΣ _ (R1 x) = gTRΣ True x
instance (GTRπ f, Constructor m) => GTRΣ (C1 m f)  where gTRΣ h' (M1 x) = (\case Node [] -> Leaf $ conName @m Dummy; y -> Node (bool [] [Leaf $ conName @m Dummy] h') <> y) $ gTRπ x

-- | "Generic Tree Renderer" logic on generic product type
class GTRπ (f :: Type -> Type)              where gTRπ :: f p -> Tree
instance (GTRπ f, GTRπ g) => GTRπ (f :*: g) where gTRπ (x:*:y) = gTRπ x <> gTRπ y
instance HasTR a => GTRπ (S1 m (Rec0 a))    where gTRπ (M1 (K1 x)) = (\case y@(Leaf _) -> y; y -> Node [y]) $ aTR x
instance GTRπ U1                            where gTRπ _ = Node []
