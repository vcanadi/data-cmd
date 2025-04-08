{- | Convert Form into generic type representation -}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, U1(U1), K1(K1), D1, to, Constructor (conName), Datatype, datatypeName)
import Text.Read (readEither)
import Data.Char (toLower)
import DataCmd.Core.Res
import DataCmd.Core.Trans (HasTrans (trans))
import DataCmd.Generic (GTypNm (gTypNm), TypNm (typNm), MW (mW), Dummy (Dummy))
import Control.Applicative (Alternative((<|>), empty))
import DataCmd.Former.Form (F (FPrim, F), FΣ (FΣ), FC (FC), FΠ (FΠ))


-- | Form parser that matchines leaf and reads content
readF :: forall a. (TypNm a, Read a) => F -> Res a
readF (FPrim s) = parseEither s
readF t = parseErr $ ", expecting FPrim, got: " <> show t

instance (HasFP a) => HasTrans F a where
  trans = aFP (Proxy @a)

class TypNm a => HasFP (a :: Type) where aFP :: Proxy a -> F -> Res a
instance HasFP Bool   where aFP _ = readF
instance HasFP Int    where aFP _ = readF
instance HasFP Float  where aFP _ = readF
instance HasFP Double where aFP _ = readF
instance {-# OVERLAPS #-} HasFP String where aFP _ = readF
instance {-# OVERLAPS #-} HasFP a => HasFP [a] where aFP _ (F (FΣ (FC "L") (FΠ cs))) = traverse (aFP (Proxy @a)) cs
                                                     aFP _ _                         = empty ## "Error parsing list"
instance {-# OVERLAPPABLE #-} (Generic a, GFP (Rep a), GTypNm (Rep a)) => HasFP a where aFP _ = genF

-- | Generic Form Parser
genF :: forall a. (Generic a, GFP (Rep a)) => F -> Res a
genF = fmap (to @a) . gFP (Proxy @(Rep a))

-- Parser

-- | Typeclass "GFP(Generic Form Parser)" whose instances (generic representations) know how to generate Form parser
class GFP (f :: Type -> Type)                 where gFP :: Proxy f -> F -> Res (f p)
instance (GFPΣ f, Datatype m) => GFP (D1 m f) where gFP _ (F t) = M1 <$> gFPΣ (Proxy @f) t
                                                    gFP _ t     = empty ## ("Form " <> show t <> " not matched with expected datatype " <> datatypeName @m Dummy )


-- | "Generic Form Parser" logic on generic sum type
class GFPΣ (f :: Type -> Type)                      where gFPΣ :: Proxy f -> FΣ -> Res (f p)
instance (GFPΣ f, GFPΣ g)         => GFPΣ (f :+: g) where gFPΣ _ t                          = (L1 <$> gFPΣ (Proxy @f) t) <|> (R1 <$> gFPΣ (Proxy @g) t)
instance (GFPΠ f, Constructor m) => GFPΣ (C1 m f)   where gFPΣ _ (FΣ (FC c) ts) | conEq     = M1 <$> gFPΠ (Proxy @f) ts
                                                                                | otherwise = empty ## ("Form's constructor " <> c <> " not matched with expected constructor " <> conName @m Dummy)
                                                                                where
                                                                                 conEq = fmap toLower (conName @m Dummy) == fmap toLower c

-- | "Generic Form Parser" logic on generic product type
class GFPΠ (f :: Type -> Type)                    where gFPΠ :: Proxy f -> FΠ -> Res (f p)
instance (GFPΠ f, GFPΠ g, MW f) => GFPΠ (f :*: g) where gFPΠ _ (FΠ ts)  = (:*:) <$> gFPΠ (Proxy @f) (FΠ $ take (mW (Proxy @f)) ts)
                                                                            <*> gFPΠ (Proxy @g) (FΠ $ drop (mW (Proxy @f)) ts)
instance (HasFP a) => GFPΠ (S1 m (Rec0 a))        where gFPΠ _ (FΠ [t]) = fmap (M1 . K1) (aFP (Proxy @a) t)
                                                        gFPΠ _ t        = parseErr  $ show t <> ", expecting singleton FΠ"
instance GFPΠ U1                                  where gFPΠ _ _        = pure U1

parseErr :: String -> Res a
parseErr msg = empty ## ("F err; " <> "msg: " <> msg)

parseEither :: Read a => String -> Res a
parseEither w = case readEither w of
  Left msg -> parseErr  (msg <> ";  " <> w)
  Right x -> pure x
