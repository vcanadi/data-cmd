{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}

module DataCmd.TreeSpec where


import Test.Hspec
import DataCmd.Form (pattern (:..), Form , pattern FPrim)
import Control.Monad(forM_)
import DataCmd.Tree (Tree (ND, LF))
import DataCmd.Tree.Trans
import DataCmd.Core.Trans
import Test.QuickCheck (Arbitrary(arbitrary), Gen, suchThat)
import Test.QuickCheck.Property (forAll)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Gen ( choose, vectorOf)
import Data.List.NonEmpty
import DataCmd.Common (shouldResultIn)
import DataCmd.Core.Trans (HasTrans(trnUp))

smplsFormer :: [(String, Tree, Form)]
smplsFormer =
  [
   ( "works on simple example"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [LF "300",ND [LF "310",LF "311"]]]
    , "0" :..
      [ FPrim "1"
      , "20" :.. [FPrim "21"]
      , "300" :..
        [ "310" :.. [FPrim "311"]
        ]
      ]
    )

  , ( "works on  singleton"
    , LF "1"
    , "1" :.. []
    )
  ]

-- | Subset of Tree used for Tree --> Form --> Tree property tests
arbitraryFormTree :: Gen Tree
arbitraryFormTree = f (1 :: Int)
  where
    f d = do
      con <- arbitrary `suchThat` (not . null)
      isLeaf <- (/=1) <$> choose (1,(d::Int)) -- select non-leaf node with probability 1/d
      if isLeaf
         then pure $ LF con
         else  do
           k <- choose (1::Int,8) -- Non leafs have 1 to 8 random children
           ts <- vectorOf k (f $ succ d)
           pure $ ND $ LF con :| ts



specFormer :: Spec
specFormer = do
  -- describe "treeForm" $
  --   forM_ smplsFormer $ \(testDesc, tree, form) ->
  --     it testDesc $
  --       treeForm tree  `shouldResultIn` form

  describe "id properties" $ do
    prop "Form --> Tree --> Form == id" $
      forAll (arbitrary @Form) $ \fm ->
        (trnDown @Tree @Form fm >>= trnUp @Tree @Form) `shouldResultIn` fm

    prop "Tree --> Form --> Tree == id" $
      forAll arbitraryFormTree $ \ft ->
        (trnUp @Tree @Form ft >>= trnDown @Tree @Form) `shouldResultIn` ft



spec :: Spec
spec = specFormer
