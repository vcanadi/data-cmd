{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}

module DataCmd.TreeSpec where


import Test.Hspec
import DataCmd.Form (pattern (:..), F , pattern FPrim)
import Control.Monad(forM_)
import DataCmd.Tree (Tree (ND, LF))
import DataCmd.Tree.TreeToForm (treeForm)
import DataCmd.Tree.FormToTree ()
import DataCmd.Core.Trans
import Test.QuickCheck (Arbitrary(arbitrary), Gen, suchThat)
import Test.QuickCheck.Property (forAll)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Gen ( choose, vectorOf)
import Data.List.NonEmpty
import DataCmd.Common (shouldResultIn)

smplsFormer :: [(String, Tree, F)]
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

-- | Subset of Tree used for Tree --> F --> Tree property tests
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
  describe "treeForm" $
    forM_ smplsFormer $ \(testDesc, tree, form) ->
      it testDesc $
        treeForm tree  `shouldResultIn` form

  describe "id properties" $ do
    prop "trans @F @Tree . trans @Tree @F == id" $
      forAll (arbitrary @F) $ \fm ->
        (trans @F @Tree fm >>= trans @Tree @F) `shouldResultIn` fm

    prop "trans @Tree @F . trans @F @Tree == id" $
      forAll arbitraryFormTree $ \ft ->
        (trans @Tree @F ft >>= trans @F @Tree) `shouldResultIn` ft



spec :: Spec
spec = specFormer
