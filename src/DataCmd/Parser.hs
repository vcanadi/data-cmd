{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DataCmd.Parser where

import Data.Kind (Type)

import Data.Proxy
import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, U1(U1), K1(K1), D1, to, Constructor (conName), Datatype, Selector (..))
import Data.List (intercalate)
import Text.Read (readEither)
import Data.Char (toLower)
import DataCmd.Core.Res
import DataCmd.Generic (GTypNm (gTypNm), TypNm (typNm), MW (mW), H, Dummy (Dummy))
import qualified DataCmd.Generic as DCG
import Control.Applicative (Alternative((<|>), empty))
import DataCmd.Former.Form (F (FPrim, F), FΣ (FΣ), FC (FC), FΠ (FΠ))

-- | Form parsers,
type FP a = F -> Res a
type FPΣ a = FΣ -> Res a
type FPΠ a = FΠ -> Res a

-- | Form parser that matchines leaf and reads content
readFP :: forall a. (TypNm a, Read a) => [String] -> FP a
readFP adr (FPrim s) = parseEither adr s
readFP adr t = parseErr (typNm (Proxy @a):adr) $ ", expecting LF, got: " <> show t


class TypNm a => HasF (a :: Type) where aF :: [String] -> Proxy a -> FP a
instance HasF Bool   where aF adr _ = readFP adr
instance HasF Int    where aF adr _ = readFP adr
instance HasF Float  where aF adr _ = readFP adr
instance HasF Double where aF adr _ = readFP adr
instance {-# OVERLAPS #-} HasF String where aF adr _ = readFP adr
instance {-# OVERLAPS #-} HasF a => HasF [a] where aF adr _ (F (FΣ (FC "L") (FΠ cs))) = traverse (aF adr (Proxy @a)) cs
                                                   aF _   _ _                    = empty ## "Error parsing list"
instance {-# OVERLAPPABLE #-} (Generic a, GFP (Rep a), GTypNm (Rep a)) => HasF a where aF adr _ = genFP adr

-- | Generic Form Parser
genFP :: forall a. (Generic a, GFP (Rep a)) => [String] -> FP a
genFP adr = fmap (to @a) . gFP adr (Proxy @(Rep a))

-- Parser

-- | Typeclass "GFP(Generic Form Parser)" whose instances (generic representations) know how to generate Form parser
class GFP (f :: Type -> Type)                            where gFP :: [String] -> Proxy f -> FP (f p)
instance (GFPΣ f, H f, MW f, Datatype m) => GFP (D1 m f) where gFP adr _ (F t) = fmap M1 $ gFPΣ (adr<>[gTypNm (Proxy @(D1 m f))]) (DCG.h $ Proxy @f) (mW $ Proxy @f) (Proxy @f) t


-- | "Generic Form Parser" logic on generic sum type
class GFPΣ (f :: Type -> Type)                           where gFPΣ :: [String] -> Int -> Int -> Proxy f -> FPΣ (f p)
instance (GFPΣ f, GFPΣ g)              => GFPΣ (f :+: g) where gFPΣ adr h w _ t                   = (L1 <$> gFPΣ adr h w (Proxy @f) t) <|> (R1 <$> gFPΣ adr h w (Proxy @g) t)
instance (GFPΠ f, Constructor m, MW f) => GFPΣ (C1 m f)  where gFPΣ adr h w _ (FΣ (FC s) ts) | conEq s = M1 <$> gFPΠ adr (mW $ Proxy @f) (Proxy @f) ts
                                                                                    where
                                                                                      conEq s = fmap toLower (conName @m Dummy) == fmap toLower s

-- | "Generic Form Parser" logic on generic product type
class GFPΠ (f :: Type -> Type)                         where gFPΠ :: [String] -> Int -> Proxy f -> FPΠ (f p)
instance (GFPΠ f, GFPΠ g, MW f) => GFPΠ (f :*: g)      where gFPΠ adr w _ (FΠ ts) = (:*:) <$> gFPΠ adr w (Proxy @f) (FΠ $ take (mW (Proxy @f)) ts)
                                                                                        <*> gFPΠ adr w (Proxy @g) (FΠ $ drop (mW (Proxy @f)) ts)
instance (HasF a, Selector m) => GFPΠ (S1 m (Rec0 a)) where gFPΠ adr w _ (FΠ [t]) = fmap (M1 . K1) (aF (adr<>[selName @m Dummy]) (Proxy @a) t)
                                                            gFPΠ adr _ _ t        = parseErr (adr<>[selName @m Dummy]) $ show t <> ", expecting singleton FΠ"
instance GFPΠ U1                                       where gFPΠ _   _ _ = const $ pure U1

parseErr :: [String] -> String -> Res a
parseErr adr msg = empty ## ("FP err; " <> intercalate "." adr <> "msg: " <> msg)

parseEither :: Read a => [String] -> String -> Res a
parseEither adr w = case readEither w of
  Left msg -> parseErr (adr<>[msg]) w
  Right x -> pure x
