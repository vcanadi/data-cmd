module DataCmd.Raw.NSepSpec where

import Test.Hspec
import DataCmd.Raw.NSep.RawToTree
import DataCmd.Tree.TreeToForm
import DataCmd.Tree.FormToTree
import DataCmd.Tree
import Control.Monad(forM_)

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

specLexNSep :: Spec
specLexNSep =
  describe "lexNSep '.'" $
    forM_ smplsLexNSep $ \(testDesc, raw, tree) ->
      it testDesc $
        lexNSep '.' raw `shouldBe` tree

spec :: Spec
spec = specLexNSep
