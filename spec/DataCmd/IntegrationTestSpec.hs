

module DataCmd.IntegrationTestSpec where

import Test.Hspec
import DataCmd
import DataCmd.ParserSpec
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import Control.Monad((<=<))
import DataCmd.Lexer.BrackSpec
import DataCmd.Util (Res(resRes))


spec :: Spec
spec = do
  describe "aTP with lexNSep" $ do
    let f = aTP [] (Proxy @Act) . lexNSep '.'
    it "parses NoAct correctly"  $
      f "NoAct"  `shouldResultIn` pure NoAct

    it "parses Move correctly"  $
      f "Move . 1 .. 2" `shouldResultIn` pure (Move (1,2))

    it "parses MoveX correctly"  $
      f "MoveX . 1"  `shouldResultIn` pure (MoveX 1)

    it "parses Spawn correctly"  $
      f "Spawn . 1 .. 2 . \"Player0\"" `shouldResultIn` pure (Spawn (1,2) "Player0")


  describe "aTP  with lexBrack" $ do
    let f = aTP [] (Proxy @Act) <=< lexBrack
    it "parses NoAct correctly"  $
      f "(NoAct)"  `shouldResultIn` pure NoAct

    it "parses Move correctly"  $
      f "(Move)((1)(2))" `shouldResultIn` pure (Move (1,2))

    -- it "parses MoveX correctly"  $
    --   f "MoveX . 1"  `shouldResultIn` pure (MoveX 1)

    -- it "parses Spawn correctly"  $
    --   f "Spawn . 1 .. 2 . \"Player0\"" `shouldResultIn` pure (Spawn (1,2) "Player0")



