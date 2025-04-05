module DataCmd.ParserSpec where

import Test.Hspec
import DataCmd.Parser
import DataCmd.Lexer.Tree
import DataCmd.Lexer.BrackSpec
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import DataCmd.Core.Res(Res(resRes))

data Act
 = NoAct
 | Move { moveDir :: (Int, Int) }
 | MoveX { moveX :: Int }
 | Spawn { spawnLoc :: (Int, Int), spawnName :: String  }
 deriving (Generic, Show, Eq)

spec :: Spec
spec = do
  pure ()
  -- describe "aTP" $ do
  --   it "parses NoAct correctly"  $
  --     aTP [] (Proxy @Act) (T $ TΣ (TC "NoAct") $ TΠ [])  `shouldResultIn` pure NoAct

  --   it "parses Move correctly"  $
  --     aTP [] (Proxy @Act) (T $ TΣ (TC "Move") $ TΠ [TPrim "1", TPrim "2"])  `shouldResultIn` pure (Move (1,2))

  --   it "parses MoveX correctly"  $
  --     aTP [] (Proxy @Act) (T $ TΣ (TC "Move") $ TΠ [TPrim "1" ])  `shouldResultIn` pure (MoveX 1)

  --   it "parses Spawn correctly"  $
  --     aTP [] (Proxy @Act) (T $ TΣ (TC "Spawn") $ TΠ [ T $ TΣ (TC "1") $ TΠ [TPrim "2"], TPrim "\"Player0\""])  `shouldResultIn` pure (Spawn (1,2) "Player0")


