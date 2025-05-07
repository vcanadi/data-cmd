{-# LANGUAGE PatternSynonyms #-}
module DataCmd.FormSpec where

import Test.Hspec
import GHC.Generics (Generic)
import DataCmd.Form (F(..), pattern (:..), pattern FPrim)
import DataCmd.Form.TypeToForm()
import DataCmd.Core.Trans (HasTrans(trn))
import DataCmd.Form.FormToType (aFP)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ( forAll, arbitrary, Gen, elements )
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import DataCmd.Common (shouldResultIn)

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

data X1 = X10 | X11 deriving (Show, Eq, Generic)
data X2 = X20 | X21 | X22 deriving (Show, Eq, Generic)

-- | A type that can be trnlated to F's subset
data Y1
  = Y10 { y10x1 :: X1, y10x2 :: X2 }
  | Y11 { y11y1 :: Y1 }
  deriving (Show, Eq, Generic)

-- | Subset of F used for property F --> Type(Y1) --> F property tests
arbitraryYF :: Gen F
arbitraryYF = do
  con <- elements ["Y10", "Y11"]
  if con == "Y10"
     then do
       y10x1 <- elements ["X10", "X11"]
       y10x2 <- elements ["X20", "X21", "X22"]
       pure $ con :.. [ y10x1 :.. [], y10x2 :.. [] ]
     else do
       y11y1 <- arbitraryYF
       pure $ con :.. [ y11y1 ]

instance Arbitrary Dir where arbitrary = genericArbitrary
instance Arbitrary Act where arbitrary = genericArbitrary
instance Arbitrary X1 where arbitrary = genericArbitrary
instance Arbitrary X2 where arbitrary = genericArbitrary
instance Arbitrary Y1 where arbitrary = genericArbitrary


spec :: Spec
spec = do
  describe "aFP" $ do
    it "parses primitive correctly"  $
      aFP @Int (FPrim "1")  `shouldResultIn` 1

  describe "trn @F @Act" $ do
    it "parses List correctly" $
      trn @F @[Int] ("L" :.. [FPrim "1", FPrim "2"]) `shouldResultIn` [1, 2]

    it "parses Prod correctly" $
      trn @F @Prod ("Prod" :.. [FPrim "1", "L" :.. [FPrim "2", FPrim "3"]]) `shouldResultIn` Prod 1 [2, 3]

    it "parses NoAct correctly"  $
      trn @F @Act ("NoAct" :.. [])  `shouldResultIn` NoAct

    it "parses Move correctly"  $
      trn @F @Act ("MoveDir" :.. ["Dir" :.. [FPrim "1", FPrim "2"]])  `shouldResultIn` MoveDir (Dir 1 2)

    it "parses MoveX correctly"  $
      trn @F @Act ("MoveX" :.. [FPrim "1" ])  `shouldResultIn` MoveX 1

    it "parses Spawn correctly"  $
      trn @F @Act ("Spawn" :.. [ "L" :.. [FPrim "1", FPrim "2"], FPrim "Player0"])  `shouldResultIn` Spawn (1,2) "Player0"


  describe "id properties" $ do
    prop "trn @Act @F . trn @F @Act == id" $
      forAll (arbitrary @Act) $ \act ->
        (trn @Act @F act >>= trn @F @Act) `shouldResultIn` act

    prop "trn @F @Act . trn @Act @F == id" $
      forAll arbitraryYF $ \fm ->
        (trn @F @Y1 fm >>= trn @Y1 @F) `shouldResultIn` fm
