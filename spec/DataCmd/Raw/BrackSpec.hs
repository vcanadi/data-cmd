{-# LANGUAGE OverloadedLists #-}

module DataCmd.Raw.BrackSpec where

import Test.Hspec
import DataCmd.Raw.Brack.RawToTree
import DataCmd.Tree
import Control.Monad(forM_)
import DataCmd.Common (shouldResultIn)

smplsLexNormal :: [(String, String, Tree)]
smplsLexNormal =
  [ ( "works on simple example"
    , "0  1  (20 21)  ((300 301) (310 311))"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
    )
  ]

smplsLexBrack:: [(String, String, Tree)]
smplsLexBrack =
  [ ( "works on simple example"
    , "(0)(1)((20)(21))(((300)(301))((310)(311)))"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
    )

  , ( "works on singleton"
    , "0"
    , LF "0"
    )

  , ( "works on nested singleton"
    , "((0))"
    , ND [ND [LF "0"]]
    )

  , ( "works on doubly nested singleton"
    , "(((0)))"
    , ND [ND [ND [LF "0"]]]
    )
  ]

specLexNormal :: Spec
specLexNormal =
  describe "lexNormal" $
    forM_ smplsLexNormal $ \(testDesc, raw, tree) ->
      it testDesc $
        lexNormal raw `shouldResultIn` tree

specLexBrack:: Spec
specLexBrack =
  describe "lexBrack" $
    forM_ smplsLexBrack $ \(testDesc, raw, tree) ->
      it testDesc $
        lexBrack raw `shouldResultIn` tree

spec :: Spec
spec = do
  specLexNormal
  specLexBrack
