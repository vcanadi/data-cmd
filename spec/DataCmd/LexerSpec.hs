module DataCmd.LexerSpec where

import Test.Hspec
import DataCmd.Lexer
import DataCmd.Lexer.NSep
import Control.Monad(forM_)

samples :: [(String, String, Tree)]
samples =
  [ ( "works on simple example"
    , "0 . 1 . 20 .. 21 . 300 ... 301 .. 310 ... 311"
    , Node [Leaf "0",Leaf "1",Node [Leaf "20",Leaf "21"],Node [Node [Leaf "300",Leaf "301"],Node [Leaf "310",Leaf "311"]]]
    )

  , ( "works on simple example without spaces"
    , "0.1.20..21.300...301..310...311"
    , Node [Leaf "0",Leaf "1",Node [Leaf "20",Leaf "21"],Node [Node [Leaf "300",Leaf "301"],Node [Leaf "310",Leaf "311"]]]
    )
  ]

samplesSpec :: Spec
samplesSpec =
  describe "lexNSep '.'" $
    forM_ samples $ \(testDesc, raw, tree) ->
      it testDesc $
        lexNSep '.' raw `shouldBe` tree

spec :: Spec
spec = samplesSpec
