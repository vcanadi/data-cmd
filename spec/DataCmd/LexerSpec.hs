module DataCmd.LexerSpec where

import Test.Hspec
import DataCmd.Lexer
import DataCmd.Lexer.NSep
import Control.Monad(forM_)

samples :: [(String, String, Tree)]
samples =
  [ ( "works on simple example"
    , "0 . 1 . 20 .. 21 . 300 ... 301 .. 310 ... 311"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
    )

  , ( "works on simple example without spaces"
    , "0.1.20..21.300...301..310...311"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
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
