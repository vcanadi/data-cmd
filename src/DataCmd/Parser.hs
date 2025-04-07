{- | Convert Form into generic type representation -}

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


-- | Form parser that matchines leaf and reads content
readF :: forall a. (TypNm a, Read a) => F -> Res a
readF (FPrim s) = parseEither s
readF t = parseErr $ ", expecting LF, got: " <> show t


class TypNm a => HasF (a :: Type) where aF :: Proxy a -> F -> Res a
instance HasF Bool   where aF _ = readF
instance HasF Int    where aF _ = readF
instance HasF Float  where aF _ = readF
instance HasF Double where aF _ = readF
instance {-# OVERLAPS #-} HasF String where aF _ = readF
instance {-# OVERLAPS #-} HasF a => HasF [a] where aF _ (F (FΣ (FC "L") (FΠ cs))) = traverse (aF (Proxy @a)) cs
                                                   aF _ _                         = empty ## "Error parsing list"
instance {-# OVERLAPPABLE #-} (Generic a, GF (Rep a), GTypNm (Rep a)) => HasF a where aF _ = genF

-- | Generic Form Parser
genF :: forall a. (Generic a, GF (Rep a)) => F -> Res a
genF = fmap (to @a) . gF (Proxy @(Rep a))

-- Parser

-- | Typeclass "GF(Generic Form Parser)" whose instances (generic representations) know how to generate Form parser
class GF (f :: Type -> Type)                            where gF :: Proxy f -> F -> Res (f p)
instance (GFΣ f, H f, MW f, Datatype m) => GF (D1 m f) where gF _ (F t) = fmap M1 $ gFΣ (DCG.h $ Proxy @f) (mW $ Proxy @f) (Proxy @f) t


-- | "Generic Form Parser" logic on generic sum type
class GFΣ (f :: Type -> Type)                         where gFΣ :: Int -> Int -> Proxy f -> FΣ -> Res (f p)
instance (GFΣ f, GFΣ g)              => GFΣ (f :+: g) where gFΣ h w _ t                          = (L1 <$> gFΣ h w (Proxy @f) t) <|> (R1 <$> gFΣ h w (Proxy @g) t)
instance (GFΠ f, Constructor m, MW f) => GFΣ (C1 m f) where gFΣ h w _ (FΣ (FC s) ts) | conEq s   = M1 <$> gFΠ (mW $ Proxy @f) (Proxy @f) ts
                                                                                     | otherwise = empty ## "Expecting constructor"
                                                                                    where
                                                                                      conEq s = fmap toLower (conName @m Dummy) == fmap toLower s

-- | "Generic Form Parser" logic on generic product type
class GFΠ (f :: Type -> Type)                        where gFΠ :: Int -> Proxy f -> FΠ -> Res (f p)
instance (GFΠ f, GFΠ g, MW f) => GFΠ (f :*: g)       where gFΠ w _ (FΠ ts) = (:*:) <$> gFΠ w (Proxy @f) (FΠ $ take (mW (Proxy @f)) ts)
                                                                                        <*> gFΠ w (Proxy @g) (FΠ $ drop (mW (Proxy @f)) ts)
instance (HasF a, Selector m) => GFΠ (S1 m (Rec0 a)) where gFΠ w _ (FΠ [t]) = fmap (M1 . K1) (aF (Proxy @a) t)
                                                           gFΠ _ _  t       = parseErr  $ show t <> ", expecting singleton FΠ"
instance GFΠ U1                                      where gFΠ _ _  _       = pure U1

parseErr :: String -> Res a
parseErr msg = empty ## ("F err; " <> "msg: " <> msg)

parseEither :: Read a => String -> Res a
parseEither w = case readEither w of
  Left msg -> parseErr  (msg <> ";  " <> w)
  Right x -> pure x
