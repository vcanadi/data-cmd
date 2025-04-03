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
-- import DataCmd.Lexer
import DataCmd.Util
import DataCmd.Generic (GTypNm (gTypNm), TypNm (typNm), MW (mW), H, Dummy (Dummy))
import qualified DataCmd.Generic as DCG
import Control.Applicative (Alternative((<|>), empty))
import DataCmd.Tree (T (TPrim, T), TΣ (TΣ), TC (TC), TΠ (TΠ))

-- Parser
--
-- | Tree parsers,
type TP a = T -> Res a
type TPΣ a = TΣ -> Res a
type TPΠ a = TΠ -> Res a

-- | Tree parser that matchines leaf and reads content
readTP :: forall a. (TypNm a, Read a) => [String] -> TP a
readTP adr (TPrim s) = parseEither adr s
readTP adr t = parseErr (typNm (Proxy @a):adr) $ ", expecting LF, got: " <> show t


class TypNm a => HasTP (a :: Type) where aTP :: [String] -> Proxy a -> TP a
instance HasTP Bool   where aTP adr _ = readTP adr
instance HasTP Int    where aTP adr _ = readTP adr
instance HasTP Float  where aTP adr _ = readTP adr
instance HasTP Double where aTP adr _ = readTP adr
instance {-# OVERLAPS #-} HasTP String where aTP adr _ = readTP adr
instance {-# OVERLAPS #-} HasTP a => HasTP [a] where aTP adr _ (T (TΣ (TC "L") (TΠ cs))) = traverse (aTP adr (Proxy @a)) cs
                                                     aTP _   _ _                    = empty ## "Error parsing list"
instance {-# OVERLAPPABLE #-} (Generic a, GTP (Rep a), GTypNm (Rep a)) => HasTP a where aTP adr _ = genTP adr

-- | Generic Tree Parser
genTP :: forall a. (Generic a, GTP (Rep a)) => [String] -> TP a
genTP adr = fmap (to @a) . gTP adr (Proxy @(Rep a))

-- Parser

-- | Typeclass "GTP(Generic Tree Parser)" whose instances (generic representations) know how to generate tree parser
class GTP (f :: Type -> Type)                            where gTP :: [String] -> Proxy f -> TP (f p)
instance (GTPΣ f, H f, MW f, Datatype m) => GTP (D1 m f) where gTP adr _ (T t) = fmap M1 $ gTPΣ (adr<>[gTypNm (Proxy @(D1 m f))]) (DCG.h $ Proxy @f) (mW $ Proxy @f) (Proxy @f) t


-- | "Generic Tree Parser" logic on generic sum type
class GTPΣ (f :: Type -> Type)                           where gTPΣ :: [String] -> Int -> Int -> Proxy f -> TPΣ (f p)
instance (GTPΣ f, GTPΣ g)              => GTPΣ (f :+: g) where gTPΣ adr h w _ t                   = (L1 <$> gTPΣ adr h w (Proxy @f) t) <|> (R1 <$> gTPΣ adr h w (Proxy @g) t)
instance (GTPΠ f, Constructor m, MW f) => GTPΣ (C1 m f)  where gTPΣ adr h w _ (TΣ (TC s) ts) | conEq s = M1 <$> gTPΠ adr (mW $ Proxy @f) (Proxy @f) ts
                                                                                    where
                                                                                      conEq s = fmap toLower (conName @m Dummy) == fmap toLower s

-- | "Generic Tree Parser" logic on generic product type
class GTPΠ (f :: Type -> Type)                         where gTPΠ :: [String] -> Int -> Proxy f -> TPΠ (f p)
instance (GTPΠ f, GTPΠ g, MW f) => GTPΠ (f :*: g)      where gTPΠ adr w _ (TΠ ts) = (:*:) <$> gTPΠ adr w (Proxy @f) (TΠ $ take (mW (Proxy @f)) ts)
                                                                                        <*> gTPΠ adr w (Proxy @g) (TΠ $ drop (mW (Proxy @f)) ts)
instance (HasTP a, Selector m) => GTPΠ (S1 m (Rec0 a)) where gTPΠ adr w _ (TΠ [t]) = fmap (M1 . K1) (aTP (adr<>[selName @m Dummy]) (Proxy @a) t)
                                                             gTPΠ adr _ _ t        = parseErr (adr<>[selName @m Dummy]) $ show t <> ", expecting singleton TΠ"
instance GTPΠ U1                                       where gTPΠ _   _ _ = const $ pure U1

parseErr :: [String] -> String -> Res a
parseErr adr msg = empty ## ("TP err; " <> intercalate "." adr <> "msg: " <> msg)

parseEither :: Read a => [String] -> String -> Res a
parseEither adr w = case readEither w of
  Left msg -> parseErr (adr<>[msg]) w
  Right x -> pure x
