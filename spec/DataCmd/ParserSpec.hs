module DataCmd.ParserSpec where

import Test.Hspec
import DataCmd.Parser
import DataCmd.Lexer
import DataCmd.Lexer.BrackSpec
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import DataCmd.Util (Res(resRes))

data Act
 = NoAct
 | Move { moveDir :: (Int, Int) }
 | MoveX { moveX :: Int }
 | Spawn { spawnLoc :: (Int, Int), spawnName :: String  }
 deriving (Generic, Show, Eq)

spec :: Spec
spec = do
  describe "aTP" $ do
    it "parses NoAct correctly"  $
      aTP [] (Proxy @Act) (ND [LF "NoAct"])  `shouldResultIn` pure NoAct

    it "parses Move correctly"  $
      aTP [] (Proxy @Act) (ND [LF "Move", ND [LF "1", LF "2"] ])  `shouldResultIn` pure (Move (1,2))

    it "parses MoveX correctly"  $
      aTP [] (Proxy @Act) (ND [LF "MoveX", LF "1" ])  `shouldResultIn` pure (MoveX 1)

    it "parses Spawn correctly"  $
      aTP [] (Proxy @Act) (ND [LF "Spawn", ND [LF "1", LF "2"] , LF "\"Player0\""])  `shouldResultIn` pure (Spawn (1,2) "Player0")


