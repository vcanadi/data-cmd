

module DataCmd.IntegrationTestSpec where

import Test.Hspec
-- import DataCmd.ParserSpec
import DataCmd.Form
import DataCmd.Tree.TreeToForm ()
import DataCmd.Form.FormToType ()
import Control.Monad((>=>))
import DataCmd.Core.Res (Res(resRes))
import DataCmd.Raw.NSep(DotRaw (DotRaw) )
import DataCmd.Raw.Brack( BrackRaw (BrackRaw), NormalRaw(NormalRaw))
import DataCmd.Raw.Brack.RawToTree ()
import DataCmd.Raw.NSep.RawToTree ()
import DataCmd.Tree(Tree)
import DataCmd.Core.Trans (HasTrans(trn))
import Control.Arrow ((>>>))
import DataCmd.FormSpec

spec :: Spec
spec = do
  describe "aFP with lexNSep" $ do
    let f =  DotRaw
         >>> trn @DotRaw @Tree
         >=> trn @Tree @F
         >=> trn @F @Act

    it "parses NoAct correctly"  $
      f "NoAct"  `shouldSatisfy` (resRes >>> (== Just NoAct))

    it "parses Move correctly"  $
      f "MoveDir . Dir .. 1 .. 2 " `shouldSatisfy` (resRes >>> (== Just (MoveDir (Dir 1 2))))

    it "parses MoveX correctly"  $
      f "MoveX . 1"  `shouldSatisfy` (resRes >>> (== Just (MoveX 1)))

    it "parses Spawn correctly"  $
      f "Spawn . L .. 1 .. 2 . Player0" `shouldSatisfy` (resRes >>> (== Just (Spawn (1,2) "Player0")))


  describe "aTP  with lexBrack" $ do
    let f = BrackRaw
         >>> trn @BrackRaw @Tree
         >=> trn @Tree @F
         >=> trn @F @Act
    it "parses NoAct correctly"  $
      f "(NoAct)"  `shouldSatisfy` (resRes >>> (== Just  NoAct))

    it "parses Move correctly"  $
      f "(MoveDir)((Dir)(1)(2))" `shouldSatisfy` (resRes >>> (== Just (MoveDir (Dir 1 2))))

    it "parses MoveX correctly"  $
      f "(MoveX)(1)"  `shouldSatisfy` (resRes >>> (== Just (MoveX 1)))

    it "parses Spawn correctly"  $
      f "(Spawn)((L)(1)(2))(Player0)" `shouldSatisfy` (resRes >>> (== Just (Spawn (1,2) "Player0")))

  describe "aTP  with lexNorma" $ do
    let f = NormalRaw
         >>> trn @NormalRaw @Tree
         >=> trn @Tree @F
         >=> trn @F @Act
    pure ()
    -- it "parses NoAct correctly"  $
    --   f "NoAct"  `shouldSatisfy` (resRes >>> (== Just  NoAct))

    -- it "parses Move correctly"  $
    --   f "MoveDir (Dir 1 2)" `shouldSatisfy` (resRes >>> (== Just (MoveDir (Dir 1 2))))

    -- it "parses MoveX correctly"  $
    --   f "(MoveX)(1)"  `shouldSatisfy` (resRes >>> (== Just (MoveX 1)))

    -- it "parses Spawn correctly"  $
    --   f "(Spawn)(((,))(1)(2))(\"Player0\")" `shouldSatisfy` (resRes >>> (== Just (Spawn (1,2) "Player0")))

