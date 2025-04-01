

module DataCmd.IntegrationTestSpec where

import Test.Hspec
import DataCmd
import DataCmd.ParserSpec
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)


spec :: Spec
spec = do
  describe "aTP [] . lexNSep '.'" $ do
    let f = aTP [] (Proxy @Act) . lexNSep '.'
    it "parses NoAct correctly"  $
      f "NoAct"  `shouldBe` Right NoAct

    it "parses Move correctly"  $
      f "Move . 1 .. 2" `shouldBe` Right (Move (1,2))

    it "parses MoveX correctly"  $
      f "MoveX . 1"  `shouldBe` Right (MoveX 1)

    it "parses Spawn correctly"  $
      f "Spawn . 1 .. 2 . \"Player0\"" `shouldBe` Right (Spawn (1,2) "Player0")



