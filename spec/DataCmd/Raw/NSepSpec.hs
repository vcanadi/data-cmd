{-# LANGUAGE OverloadedLists #-}

module DataCmd.Raw.NSepSpec where

import Test.Hspec
import DataCmd.Raw.NSep.RawToTree
import DataCmd.Tree.TreeToForm ()
import DataCmd.Tree.FormToTree ()
import DataCmd.Tree
import Control.Monad(forM_)
import DataCmd.Raw.NSep (DotRaw (DotRaw))
import DataCmd.Raw.NSep.RawToTree ()
import DataCmd.Raw.NSep.TreeToRaw ()
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, Arbitrary (..), Gen, suchThat, choose, vectorOf)
import DataCmd.Core.Trans
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Char (isAlphaNum)
import DataCmd.Common (shouldResultIn)
import Data.List (intercalate)

smplsLexNSep :: [(String, String, Tree)]
smplsLexNSep =
  [ ( "works on simple example"
    , "0 . 1 . 20 .. 21 . 300 ... 301 .. 310 ... 311"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
    )

  , ( "works on simple example without spaces"
    , "0.1.20..21.300...301..310...311"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
    )

  , ( "works on  singleton"
    , "1"
    , LF "1"
    )

  , ( "works on nested singleton"
    , "20..21"
    , ND [ND [LF "20",LF "21"]]
    )

  , ( "works on double nested singleton"
    , "300...301"
    , ND [ ND [ND [LF "300",LF "301"]]]
    )
  ]

-- | Input for Tree --> Raw --> Tree === id property
arbitraryTree :: Gen Tree
arbitraryTree = f (1 :: Int)
  where
    f d = do
      con <- arbitrary `suchThat` ((&&) <$> not . null <*> all isAlphaNum)
      isLeaf <- (/=1) <$> choose (1,(d::Int)) -- select non-leaf node with probability 1/d
      if isLeaf
         then pure $ LF con
         else  do
           k <- choose (1::Int,8) -- Non leafs have 1 to 8 random children
           ts <- vectorOf k (f $ succ d)
           pure $ ND $ LF con :| ts

-- | Input for Raw --> Tree --> Raw === id property
arbitraryDot :: Gen String
arbitraryDot = f (1 :: Int)
  where
    f d = do
      s <- arbitrary `suchThat` ((&&) <$> not . null <*> all isAlphaNum)
      isLeaf <- (/=1) <$> choose (1,(d::Int)) -- select non-leaf node with probability 1/d
      if isLeaf
         then pure $ s
         else  do
           k <- choose (1::Int,8) -- Non leafs have 1 to 8 random children
           ts <- vectorOf k (f $ succ d)
           pure $ intercalate (replicate d '.') ts

specLexNSep :: Spec
specLexNSep = do
  describe "lexNSep '.'" $
    forM_ smplsLexNSep $ \(testDesc, raw, tree) ->
      it testDesc $
        lexNSep '.' raw `shouldBe` tree

  describe "id properties" $ do
    prop "trn @Tree @DotRaw . trn @DotRaw @Tree == id" $
      forAll (arbitraryTree) $ \t ->
        (trn @Tree @DotRaw t >>= trn @DotRaw @Tree) `shouldResultIn` t

    prop "trn @DotRaw @Tree . trn @Tree @DotRaw == id" $
      forAll arbitraryDot $ \raw ->
        (trn @DotRaw @Tree (DotRaw raw) >>= trn @Tree @DotRaw) `shouldResultIn` DotRaw raw


spec :: Spec
spec = specLexNSep
