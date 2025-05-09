{-# LANGUAGE PatternSynonyms #-}

module DataCmd.FormSpec where

import Test.Hspec
import GHC.Generics (Generic)
import DataCmd.Form (Form(..), pattern (:..), pattern FPrim, FC, FΠ)
import DataCmd.Form.TypeToForm()
import DataCmd.Core.Trans (HasTrans(trnUp, trnDown))
import DataCmd.Form.Trans
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ( forAll, arbitrary, Gen, elements )
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import DataCmd.Common (shouldResultIn)


-- | Direction
data Dir = Dir Int Int
 deriving (Generic, Show, Eq)

-- | Position
data Pos = Pos Int Int
 deriving (Generic, Show, Eq)

data Prod = Prod {prodA :: Int, prodB :: [Float] }
 deriving (Generic, Show, Eq)

-- | Line/segment in space
data Line  = Line Pos Pos
 deriving (Generic, Show, Eq)

data Alg = AlgPoss [Pos]
         | AlgLines [Line]
 deriving (Generic, Show, Eq)

-- | Example type with various constructors for testing
data Act
 = NoAct
 | MoveDir { moveDir  :: Dir } -- ^ Move player by direction vector
 | MoveX { moveX :: Int } -- ^ Move x coordinate by some amount
 | Spawn { spawnLoc :: Pos, spawnName :: String  } -- ^ Spawn object at specific position
 | Rope { ropeLine :: Line } -- ^  Rope from point A to point B
 | Chain { chainChain :: [Pos] } -- ^ Chain spanning points
 | SpawnAlg { spawnAlg :: Alg, spawnAlgName :: String } -- ^ Spawn command with algebraic position description
 deriving (Generic, Show, Eq)

data X1 = X10 | X11 deriving (Show, Eq, Generic)
data X2 = X20 | X21 | X22 deriving (Show, Eq, Generic)

-- | A type that can be trnlated to F's subset
data Y1
  = Y10 { y10x1 :: X1, y10x2 :: X2 }
  | Y11 { y11y1 :: Y1 }
  deriving (Show, Eq, Generic)

-- | Subset of Form used for property Form --> Type(Y1) --> Form property tests
arbitraryYF :: Gen Form
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
instance Arbitrary Pos where arbitrary = genericArbitrary
instance Arbitrary Line where arbitrary = genericArbitrary
instance Arbitrary Alg where arbitrary = genericArbitrary
instance Arbitrary Act where arbitrary = genericArbitrary
instance Arbitrary X1 where arbitrary = genericArbitrary
instance Arbitrary X2 where arbitrary = genericArbitrary
instance Arbitrary Y1 where arbitrary = genericArbitrary


spec :: Spec
spec = do
  -- describe "trnUp @Form @Int" $ do
    -- it "parses primitive correctly"  $
    --   tr @Int (FPrim "1")  `shouldResultIn` 1

  describe "trnUp @Form @Act" $ do
    it "parses List correctly" $
      trnUp @Form @[Int] ("L" :.. [FPrim "1", FPrim "2"]) `shouldResultIn` [1, 2]

    it "parses Prod correctly" $
      trnUp @Form @Prod ("Prod" :.. [FPrim "1", "L" :.. [FPrim "2", FPrim "3"]]) `shouldResultIn` Prod 1 [2, 3]

    it "parses NoAct correctly"  $
      trnUp @Form @Act ("NoAct" :.. [])  `shouldResultIn` NoAct

    it "parses Move correctly"  $
      trnUp @Form @Act ("MoveDir" :.. ["Dir" :.. [FPrim "1", FPrim "2"]])  `shouldResultIn` MoveDir (Dir 1 2)

    it "parses MoveX correctly"  $
      trnUp @Form @Act ("MoveX" :.. [FPrim "1" ])  `shouldResultIn` MoveX 1

    it "parses Spawn correctly"  $
      trnUp @Form @Act ("Spawn" :.. [ "Pos" :.. [FPrim "1", FPrim "2"], FPrim "Player0"])  `shouldResultIn` Spawn (Pos 1 2) "Player0"

    it "parses Rope correctly"  $
      trnUp @Form @Act
      ( "Rope" :..
          [ "Line" :..
              [ "Pos" :.. [FPrim "1", FPrim "2"]
              , "Pos" :.. [FPrim "3", FPrim "4"]
              ]
          ]
      ) `shouldResultIn` Rope (Line (Pos 1 2) (Pos 3 4))

    it "parses Chain correctly"  $
      trnUp @Form @Act ("Chain" :.. [ "L" :.. [ "Pos" :.. [FPrim "1", FPrim "2"], "Pos" :.. [FPrim "3", FPrim "4"], "Pos" :.. [FPrim "5", FPrim "6"]]])
      `shouldResultIn` Chain [Pos 1 2, Pos 3 4, Pos 5 6 ]

    it "parses SpawnAlg correctly"  $
      trnUp @Form @Act
      ( "SpawnAlg" :..
          [ "AlgPoss" :..
              ["L" :..
                  [ "Pos" :.. [FPrim "1", FPrim "2"]
                  , "Pos" :.. [FPrim "3", FPrim "4"]
                  , "Pos" :.. [FPrim "5", FPrim "6"]
                  ]
              ]
          , FPrim "Wall"
          ]
      ) `shouldResultIn` SpawnAlg (AlgPoss [Pos 1 2, Pos 3 4, Pos 5 6 ]) "Wall"


  describe "id properties" $ do
    prop "trn @Act @Form . trn @Form @Act == id" $
      forAll (arbitrary @Act) $ \act ->
        (trnDown @Form @Act act >>= trnUp @Form @Act) `shouldResultIn` act

    prop "trn @Form @Act . trn @Act @Form == id" $
      forAll arbitraryYF $ \fm ->
        (trnUp @Form @Y1 fm >>= trnDown @Form @Y1) `shouldResultIn` fm
