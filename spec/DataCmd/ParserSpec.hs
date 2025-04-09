{-# LANGUAGE PatternSynonyms #-}
module DataCmd.ParserSpec where

import Test.Hspec
import DataCmd.Parser
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import DataCmd.Core.Res(Res(resRes))
import DataCmd.Former.Form (F(..), pattern (:..), pattern FPrim)
import DataCmd.Core.Trans (HasTrans(trans))

shouldResultIn :: (Show t, Eq t) => Res t -> t -> Expectation
shouldResultIn a b = resRes a `shouldBe` Just b

data Dir = Dir Int Int
 deriving (Generic, Show, Eq)

data Prod = Prod {prodA :: Int, prodB :: [Float] }
 deriving (Generic, Show, Eq)

data Act
 = NoAct
 | MoveDir { moveDir  :: Dir }
 | MoveX { moveX :: Int }
 | Spawn { spawnLoc :: (Int, Int), spawnName :: String  }
 deriving (Generic, Show, Eq)

spec :: Spec
spec = do
  pure ()
  -- describe "genFP" $ do
    -- it "parses primitive correctly"  $
    --   genFP [] (FF "1" [])  `shouldResultIn` 1

  describe "trans @F @Act" $ do

    it "parses List correctly" $
      trans @F @[Int] ("L" :.. [FPrim "1", FPrim "2"]) `shouldResultIn` [1, 2]

    it "parses Prod correctly" $
      trans @F @Prod ("Prod" :.. [FPrim "1", "L" :.. [FPrim "2", FPrim "3"]]) `shouldResultIn` Prod 1 [2, 3]

    it "parses NoAct correctly"  $
      trans @F @Act ("NoAct" :.. [])  `shouldResultIn` NoAct

    it "parses Move correctly"  $
      trans @F @Act ("MoveDir" :.. ["Dir" :.. [FPrim "1", FPrim "2"]])  `shouldResultIn` MoveDir (Dir 1 2)

    it "parses MoveX correctly"  $
      trans @F @Act ("MoveX" :.. [FPrim "1" ])  `shouldResultIn` MoveX 1

    it "parses Spawn correctly"  $
      trans @F @Act ("Spawn" :.. [ "(,)" :.. [FPrim "1", FPrim "2"], FPrim "\"Player0\""])  `shouldResultIn` Spawn (1,2) "Player0"


