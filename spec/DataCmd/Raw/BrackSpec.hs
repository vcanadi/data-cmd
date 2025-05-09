{-# LANGUAGE OverloadedLists #-}

module DataCmd.Raw.BrackSpec where

import Test.Hspec
import DataCmd.Raw.Brack.Trans
import DataCmd.Tree
import Control.Monad(forM_)
import DataCmd.Common (shouldResultIn)

smplsLexBrackPlus :: [(String, String, Tree)]
smplsLexBrackPlus =
  [ ( "works on simple example"
    , "0  1  (20 21)  ((300 301) (310 311))"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
    )

  , ( "works on Act example (Spawn)"
    , "Spawn (Pos 1 2) Player0"
    , ND [ LF "Spawn"
         , ND [LF "Pos", LF "1",LF "2"]
         , LF "Player0"
         ]
    )

  , ( "works on Act example (Rope)"
    , "Rope (Line (Pos 1 2) (Pos 3 4 ) )"
    , ND [ LF "Rope"
         , ND [ LF "Line"
              , ND [LF "Pos", LF "1",LF "2"]
              , ND [LF "Pos", LF "3",LF "4"]
              ]
         ]
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
    , "(0)"
    , ND [LF "0"]
    )

  , ( "works on double nested singleton"
    , "((0))"
    , ND [ND [LF "0"]]
    )

  , ( "works on triple nested singleton"
    , "(((0)))"
    , ND [ND [ND [LF "0"]]]
    )

  , ( "works on Act example (Spawn)"
    , "(Spawn)((Pos)(1)(2))(Player0)"
    , ND [ LF "Spawn"
         , ND [LF "Pos", LF "1",LF "2"]
         , LF "Player0"
         ]
    )

  , ( "works on Act example (Rope)"
    , "(Rope)((Line)((Pos)(1)(2))((Pos)(3)(4)))"
    , ND [ LF "Rope"
         , ND [ LF "Line"
              , ND [LF "Pos", LF "1",LF "2"]
              , ND [LF "Pos", LF "3",LF "4"]
              ]
         ]
    )
  ]

specLexBrackPlus :: Spec
specLexBrackPlus =
  describe "lexBrackPlus" $
    forM_ smplsLexBrackPlus $ \(testDesc, raw, tree) ->
      it testDesc $
        lexBrackPlus raw `shouldResultIn` tree

specLexBrack:: Spec
specLexBrack =
  describe "lexBrack" $
    forM_ smplsLexBrack $ \(testDesc, raw, tree) ->
      it testDesc $
        lexBrack raw `shouldResultIn` tree


spec :: Spec
spec = do
  specLexBrackPlus
  specLexBrack
