
module DataCmd.Common where

import DataCmd.Core.Res (Res (resRes))
import Test.Hspec (Expectation, shouldBe)

shouldResultIn :: (Show t, Eq t) => Res t -> t -> Expectation
shouldResultIn a b = resRes a `shouldBe` Just b
