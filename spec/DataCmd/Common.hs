{-# OPTIONS_GHC -Wno-orphans #-}

module DataCmd.Common where

import DataCmd.Core.Res (Res (resRes))
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Arbitrary (..))
import DataCmd.Form (FC, FΠ, F)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

-- Orphan Arbitrary instances for Form

instance Arbitrary FC where arbitrary = genericArbitrary
instance Arbitrary FΠ where arbitrary = genericArbitrary
instance Arbitrary F where arbitrary = genericArbitrary


shouldResultIn :: (Show t, Eq t) => Res t -> t -> Expectation
shouldResultIn a b = resRes a `shouldBe` Just b
