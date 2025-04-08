

module DataCmd.IntegrationTestSpec where

import Test.Hspec
-- import DataCmd.ParserSpec
import DataCmd.Former.Form
import DataCmd.Former ()
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import Control.Monad((<=<), (>=>))
import DataCmd.Core.Res (Res(resRes))
import DataCmd.Lexer (DotLexer (DotLexer))
import DataCmd.Lexer.Tree (Tree)
import DataCmd.Core.Trans (HasTrans(trans))
import Control.Arrow ((>>>))
import DataCmd.Parser (HasFP(aFP))


data Dir = Dir Int Int
 deriving (Generic, Show, Eq)

data Prod = Prod {prodA :: Int, prodB :: [Float] }
 deriving (Generic, Show, Eq)

data Act
 = NoAct
 | MoveDir { moveDir  :: Dir }
 | MoveX { moveX :: Int }
 | Spawn { spawnLoc :: (Int, Int), spawnName :: String  }
 deriving (Generic, Show, Eq)

spec :: Spec
spec = do
  describe "aFP with lexNSep" $ do
    let f =  DotLexer
         >>> trans @DotLexer @Tree
         >=> trans @Tree @F
         >=> trans @F @Act

    it "parses NoAct correctly"  $
      f "NoAct"  `shouldSatisfy` (resRes >>> (== Just NoAct))

    -- it "parses Move correctly"  $
    --   f "Move . 1 .. 2" `shouldSatisfy` (resRes >>> (== MoveDir (Dir 1 2))

    -- it "parses MoveX correctly"  $
    --   f "MoveX . 1"  `shouldResultIn` pure (MoveX 1)

    -- it "parses Spawn correctly"  $
    --   f "Spawn . 1 .. 2 . \"Player0\"" `shouldResultIn` pure (Spawn (1,2) "Player0")


  -- describe "aTP  with lexBrack" $ do
  --   let f = aTP [] (Proxy @Act) <=< lexBrack
  --   it "parses NoAct correctly"  $
  --     f "(NoAct)"  `shouldResultIn` pure NoAct

  --   it "parses Move correctly"  $
  --     f "(Move)((1)(2))" `shouldResultIn` pure (Move (1,2))

    -- it "parses MoveX correctly"  $
    --   f "MoveX . 1"  `shouldResultIn` pure (MoveX 1)

    -- it "parses Spawn correctly"  $
    --   f "Spawn . 1 .. 2 . \"Player0\"" `shouldResultIn` pure (Spawn (1,2) "Player0")



