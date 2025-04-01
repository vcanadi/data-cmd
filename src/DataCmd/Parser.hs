{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DataCmd.Parser where

import Data.Kind (Type)

import Data.Proxy
import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, U1(U1), K1(K1), D1, to, Constructor (conName), Datatype, Selector (..), from)
import Data.List (intercalate)
import Text.Read (readEither)
import Data.Char (toLower)
import Data.Bool (bool)
import DataCmd.Lexer
import DataCmd.Util
import DataCmd.Generic (GTypNm (gTypNm), TypNm (typNm), MW (mW), H, Dummy (Dummy))
import qualified DataCmd.Generic as DCG

-- Parser
--
-- | Tree parser,
type TP a = Tree -> Res a

-- | Tree parser that matchines leaf and reads content
readTP :: forall a. (TypNm a, Read a) => [String] -> TP a
readTP adr (LF s) = parseEither adr s
readTP adr t = parseErr (typNm (Proxy @a):adr) $ ", expecting LF, got: " <> showT t


class TypNm a => HasTP (a :: Type) where aTP :: [String] -> Proxy a -> TP a
instance HasTP Bool   where aTP adr _ = readTP adr
instance HasTP Int    where aTP adr _ = readTP adr
instance HasTP Float  where aTP adr _ = readTP adr
instance HasTP Double where aTP adr _ = readTP adr
instance {-# OVERLAPS #-} HasTP String where aTP adr _ = readTP adr
instance {-# OVERLAPS #-} HasTP a => HasTP [a] where aTP adr _ = \case (ND cs) -> traverse (aTP adr (Proxy @a)) cs
                                                                       _         -> Left [["Error parsing list"]]
instance {-# OVERLAPPABLE #-} (Generic a, GTP (Rep a), GTypNm (Rep a)) => HasTP a where aTP adr _ = genTP adr

-- | Generic Tree Parser
genTP :: forall a. (Generic a, GTP (Rep a)) => [String] -> TP a
genTP adr = fmap (to @a) . gTP adr (Proxy @(Rep a))

-- Parser

-- | Typeclass "GTP(Generic Tree Parser)" whose instances (generic representations) know how to generate tree parser
class GTP (f :: Type -> Type)                            where gTP :: [String] -> Proxy f -> TP (f p)
instance (GTPΣ f, H f, MW f, Datatype m) => GTP (D1 m f) where gTP adr _ = fmap M1 . gTPΣ (adr<>[gTypNm (Proxy @(D1 m f))]) (DCG.h $ Proxy @f) (mW $ Proxy @f) (Proxy @f)

-- Constraints aliases for brevity
--


-- | "Generic Tree Parser" logic on generic sum type
class GTPΣ (f :: Type -> Type)                           where gTPΣ :: [String] -> Int -> Int -> Proxy f -> TP (f p)
instance (GTPΣ f, GTPΣ g)              => GTPΣ (f :+: g) where gTPΣ adr h w _ t = (L1 <$> gTPΣ adr h w (Proxy @f) t) `resOR` (R1 <$> gTPΣ adr h w (Proxy @g) t)
instance (GTPπ f, Constructor m, MW f) => GTPΣ (C1 m f)  where gTPΣ adr h w _ t = if h == 1
                                                                                  then M1 <$> gTPπ adr (mW $ Proxy @f) (Proxy @f) t -- ^ Ignore constructor when type has a single constructor
                                                                                  else
                                                                                  case t of
                                                                                    ND (LF s:ts) | conEq s -> M1 <$> gTPπ adr (mW $ Proxy @f) (Proxy @f) (ND ts)
                                                                                    -- | Allow replacing double nest with single in case of enum (w==0)
                                                                                    (LF s)         | conEq s && w == 0 -> M1 <$> gTPπ adr (mW $ Proxy @f) (Proxy @f) (ND [])
                                                                                                   | otherwise -> parseErr adrWithCon $
                                                                                                                    showT t <> ", left leaf:(" <> s <> ") does not match expected constructor"
                                                                                    _ -> parseErr adrWithCon $ showT t <> ", expecting constructor name in left leaf"
                                                                                    where
                                                                                      adrWithCon = adr <> [conName @m Dummy]
                                                                                      conEq s = fmap toLower (conName @m Dummy) == fmap toLower s

-- | "Generic Tree Parser" logic on generic product type
class GTPπ (f :: Type -> Type)                         where gTPπ :: [String] -> Int -> Proxy f -> TP (f p)
instance (GTPπ f, GTPπ g, MW f) => GTPπ (f :*: g)      where gTPπ adr w _ = \case t@(LF _) -> parseErr (":*:":adr) $ showT t <> ", expecting LF"
                                                                                  (ND ts) ->
                                                                                         (:*:) <$> gTPπ adr w (Proxy @f) (ND $ take (mW (Proxy @f)) ts)
                                                                                               <*> gTPπ adr w (Proxy @g) (ND $ drop (mW (Proxy @f)) ts)
instance (HasTP a, Selector m) => GTPπ (S1 m (Rec0 a)) where gTPπ adr w _ = \case (ND [t])          -> fmap (M1 . K1) (aTP (adr<>[selName @m Dummy]) (Proxy @a) t)
                                                                                  t@(LF _) | w == 1 -> fmap (M1 . K1) (aTP (adr<>[selName @m Dummy]) (Proxy @a) t)
                                                                                  t                 -> parseErr (adr<>[selName @m Dummy]) $ showT t <> ", expecting singleton ND"
instance GTPπ U1                                       where gTPπ _   _ _ = const $ Right U1

parseErr :: [String] -> String -> Res a
parseErr adr msg = Left [["TP err; type: " <> intercalate "." adr <> ", msg: " <> msg]]

parseEither :: Read a => [String] -> String -> Res a
parseEither adr w = case readEither w of
  Left msg -> parseErr (adr<>[msg]) w
  Right x -> Right x


-- Renderer
--
-- | Tree parser that matchines leaf and reads content

class HasTR (a :: Type) where aTR :: a -> Tree
instance HasTR Bool   where aTR x = LF $ show x
instance HasTR Int    where aTR x = LF $ show x
instance HasTR Float  where aTR x = LF $ show x
instance HasTR Double where aTR x = LF $ show x
instance {-# OVERLAPS #-} HasTR String where aTR = LF
instance {-# OVERLAPS #-} HasTR a => HasTR [a] where aTR xs = ND $ aTR <$> xs
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
instance (GTRπ f, Constructor m) => GTRΣ (C1 m f)  where gTRΣ h' (M1 x) = (\case ND [] -> LF $ conName @m Dummy; y -> ND (bool [] [LF $ conName @m Dummy] h') <> y) $ gTRπ x

-- | "Generic Tree Renderer" logic on generic product type
class GTRπ (f :: Type -> Type)              where gTRπ :: f p -> Tree
instance (GTRπ f, GTRπ g) => GTRπ (f :*: g) where gTRπ (x:*:y) = gTRπ x <> gTRπ y
instance HasTR a => GTRπ (S1 m (Rec0 a))    where gTRπ (M1 (K1 x)) = (\case y@(LF _) -> y; y -> ND [y]) $ aTR x
instance GTRπ U1                            where gTRπ _ = ND []
