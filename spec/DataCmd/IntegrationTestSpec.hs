

module DataCmd.IntegrationTestSpec where

import Test.Hspec
-- import DataCmd.ParserSpec
import DataCmd.Former.Form
import DataCmd.Former ()
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import Control.Monad((<=<), (>=>))
import DataCmd.Core.Res (Res(resRes))
import DataCmd.Lexer (DotLexer (DotLexer), BrackLexer (BrackLexer), NormalLexer(NormalLexer))
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

    it "parses Move correctly"  $
      f "MoveDir . Dir .. 1 .. 2 " `shouldSatisfy` (resRes >>> (== Just (MoveDir (Dir 1 2))))

    it "parses MoveX correctly"  $
      f "MoveX . 1"  `shouldSatisfy` (resRes >>> (== Just (MoveX 1)))

    it "parses Spawn correctly"  $
      f "Spawn . (,) .. 1 .. 2 . \"Player0\"" `shouldSatisfy` (resRes >>> (== Just (Spawn (1,2) "Player0")))


  describe "aTP  with lexBrack" $ do
    let f = BrackLexer
         >>> trans @BrackLexer @Tree
         >=> trans @Tree @F
         >=> trans @F @Act
    it "parses NoAct correctly"  $
      f "(NoAct)"  `shouldSatisfy` (resRes >>> (== Just  NoAct))

    it "parses Move correctly"  $
      f "(MoveDir)((Dir)(1)(2))" `shouldSatisfy` (resRes >>> (== Just (MoveDir (Dir 1 2))))

    it "parses MoveX correctly"  $
      f "(MoveX)(1)"  `shouldSatisfy` (resRes >>> (== Just (MoveX 1)))

    -- it "parses Spawn correctly"  $
    --   f "(Spawn)(((,))(1)(2))(\"Player0\")" `shouldSatisfy` (resRes >>> (== Just (Spawn (1,2) "Player0")))

  describe "aTP  with lexNorma" $ do
    let f = NormalLexer
         >>> trans @NormalLexer @Tree
         >=> trans @Tree @F
         >=> trans @F @Act
    pure ()
    -- it "parses NoAct correctly"  $
    --   f "NoAct"  `shouldSatisfy` (resRes >>> (== Just  NoAct))

    -- it "parses Move correctly"  $
    --   f "MoveDir (Dir 1 2)" `shouldSatisfy` (resRes >>> (== Just (MoveDir (Dir 1 2))))

    -- it "parses MoveX correctly"  $
    --   f "(MoveX)(1)"  `shouldSatisfy` (resRes >>> (== Just (MoveX 1)))

    -- it "parses Spawn correctly"  $
    --   f "(Spawn)(((,))(1)(2))(\"Player0\")" `shouldSatisfy` (resRes >>> (== Just (Spawn (1,2) "Player0")))

