{- | Convert Form into generic type representation -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-#  OPTIONS_GHC -fno-warn-orphans #-}

module DataCmd.Form.FormToType where

import Data.Kind (Type)

import Data.Proxy
import GHC.Generics (M1 (..), (:+:) (..), (:*:) ((:*:)), Generic (Rep), C1, S1, Rec0, U1(U1), K1(K1), D1, to, Constructor (conName), Selector, selName)
import Text.Read (readEither)
import Data.Char (toLower)
import DataCmd.Core.Res
import DataCmd.Generic (GTypNm (gTypNm) , TypNm (typNm), MW (mW), Dummy (Dummy))
import Control.Applicative (Alternative((<|>), empty))
import DataCmd.Form (pattern FPrim, Form(FΣ), FC (FC), FΠ (FΠ), pattern (:..))
import Control.Arrow ((>>>))


-- | Form parser that matchines leaf and reads content
readF :: forall a. (TypNm a, Read a) => Form -> Res a
readF (FPrim s) = parseEither s
readF t = parseErr $ "expecting FPrim " <> typNm (Proxy @a) <> ", got: " <> show t


class TypNm a => HasFP (a :: Type) where aFP :: Form -> Res a
instance HasFP Bool   where aFP  = readF >>> (#< "Parsing Bool")
instance HasFP Int    where aFP  = readF >>> (#< "Parsing Int")
instance HasFP Float  where aFP  = readF >>> (#< "Parsing Float")
instance HasFP Double where aFP  = readF >>> (#< "Parsing Double")
instance {-# OVERLAPS #-} HasFP String where aFP (FPrim s) = pure s #< "Parsing String"
                                             aFP _         = parseErr $ "expecting FPrim"
instance {-# OVERLAPS #-} HasFP a => HasFP [a] where aFP ("L" :.. cs) = traverse (aFP @a) cs #< "Parsing list"
                                                     aFP _            = parseErr "Error parsing list"

instance {-# OVERLAPS #-} (HasFP a, HasFP b) => HasFP (a,b) where
  aFP ("L" :.. [x,y]) = ((,) <$> aFP x <*> aFP y) #< "Parsing tuple"
  aFP _               = parseErr "Error parsing tuple"

instance {-# OVERLAPS #-} (HasFP a, HasFP b, HasFP c) => HasFP (a,b, c) where
  aFP ("L" :.. [x,y,z]) = ((,,) <$> aFP x <*> aFP y <*> aFP z) #< "Parsing 3-tuple"
  aFP _                 = parseErr "Error parsing 3-tuple"
instance {-# OVERLAPPABLE #-} (Generic a, GFP (Rep a), GTypNm (Rep a)) => HasFP a where aFP = genFP >>> (#< "Parsing " <> gTypNm (Proxy @(Rep a)))

-- | Generic Form Parser
genFP :: forall a. (Generic a, GFP (Rep a)) => Form -> Res a
genFP = fmap (to @a) . gFP @(Rep a)

-- Parser

-- | Typeclass "GFP(Generic Form Parser)" whose instances (generic representations) know how to generate Form parser
class GFP (f :: Type -> Type)     where gFP :: Form -> Res (f p)
instance (GFPΣ f) => GFP (D1 m f) where gFP fo = M1 <$> gFPΣ @f fo

-- | Case invariant comparison
caseInvEq :: (Eq (f Char), Functor f) => f Char -> f Char -> Bool
caseInvEq c0 c1 = fmap toLower c0 == fmap toLower c1

-- | "Generic Form Parser" logic on generic sum type
class GFPΣ (f :: Type -> Type)  where gFPΣ :: Form -> Res (f p)
instance (GFPΣ f, GFPΣ g)
              => GFPΣ (f :+: g) where gFPΣ t = (L1 <$> gFPΣ @f t #< "L1")
                                           <|> (R1 <$> gFPΣ @g t #< "R1")
instance (GFPΠ f, Constructor m)
              => GFPΣ (C1 m f)  where gFPΣ (FΣ (FC c) ts)
                                          | conName @m Dummy `caseInvEq` c = M1 <$> gFPΠ @f ts #< conParseSuccMsg (conName @m Dummy)
                                          | otherwise                      = parseErr $ conParseErrMsg c (conName @m Dummy)

-- | "Generic Form Parser" logic on generic product type
class GFPΠ (f :: Type -> Type)  where gFPΠ :: FΠ -> Res (f p)
instance (GFPΠ f, GFPΠ g, MW f)
          => GFPΠ (f :*: g)     where gFPΠ (FΠ ts)  = (:*:) <$> (gFPΠ @f (FΠ $ take (mW (Proxy @f)) ts) #< "FST")
                                                            <*> (gFPΠ @g (FΠ $ drop (mW (Proxy @f)) ts) #< "SND")
instance (HasFP a, Selector m)
        => GFPΠ (S1 m (Rec0 a)) where gFPΠ (FΠ [t]) = M1 . K1 <$> aFP @a t #*<  selParseSuccMsg (selName @m Dummy)
                                      gFPΠ t        = parseErr $ selParseErrMsg (show t) (selName @m Dummy)
instance GFPΠ U1                where gFPΠ _        = pure U1

conParseErrMsg :: String -> String -> String
conParseErrMsg c0 c1 = "Form's constructor < " <> c0 <> " > not matched with expected constructor < " <> c1 <> " >"

conParseSuccMsg :: String -> String
conParseSuccMsg c = "Constructor " <> c <> " matched"

selParseErrMsg :: String -> String -> String
selParseErrMsg s0 s1 = "Selector < " <> s0 <> " > not matched with expected selector < " <> s1 <> " >"

selParseSuccMsg :: String -> String
selParseSuccMsg s = "Selector < " <> s <> " > matched"

parseErr :: String -> Res a
parseErr msg = empty #< ("Parser err: " <> msg)

parseEither :: Read a => String -> Res a
parseEither w = case readEither w of
  Left msg -> parseErr  (msg <> ";  " <> w)
  Right x -> pure x
