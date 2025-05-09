

module DataCmd.IntegrationTestSpec where

import Test.Hspec
import DataCmd.Form
import DataCmd.Tree.Trans()
import DataCmd.Form.Trans ()
import DataCmd.Core.Res (Res(resRes))
import DataCmd.Raw.NSep(DotRaw (DotRaw) )
import DataCmd.Raw.Brack( BrackRaw (BrackRaw), BrackPlusRaw(BrackPlusRaw))
import DataCmd.Raw.Brack.Trans()
import DataCmd.Raw.NSep.Trans ()
import DataCmd.Tree(Tree)
import DataCmd.Core.Trans (HasTrans(trnUp))
import Control.Arrow ((>>>))
import DataCmd.FormSpec
import DataCmd
import Control.Monad (forM_)

-- | Different raw representations and expected end result
smpls :: [(String , String, String, Act)]
smpls =
  [ ( "NoAct"
    , "(NoAct)"
    , "NoAct"
    ,  NoAct
    )
  , ( "MoveDir . Dir .. 1 .. 2 "
    , "(MoveDir)((Dir)(1)(2))"
    , "MoveDir (Dir 1 2)"
    , MoveDir (Dir 1 2)
    )
  , ( "MoveX . 1"
    , "(MoveX)(1)"
    , "MoveX 1"
    , MoveX 1
    )
  , ( "Spawn . Pos .. 1 .. 2 . Player0"
    , "(Spawn)((Pos)(1)(2))(Player0)"
    , "Spawn (Pos 1 2) (Player0)"
    , Spawn (Pos 1 2) "Player0"
    )
  , ( "Rope . Line .. Pos ... 1 ... 2 .. Pos ... 3 ... 4 "
    , "(Rope)((Line)((Pos)(1)(2))((Pos)(3)(4)))"
    , "Rope (Line (Pos 1 2) (Pos 3 4))"
    , Rope $ Line (Pos 1 2) (Pos 3 4)
    )

  , ( "Chain . L .. Pos ... 1 ... 2 .. Pos ... 3 ... 4 .. Pos ... 5 ... 6"
    , "(Chain)((L)((Pos)(1)(2))((Pos)(3)(4))((Pos)(5)(6)))"
    , "Chain (L (Pos 1 2) (Pos 3 4) (Pos 5 6))"
    , Chain [Pos 1 2, Pos 3 4, Pos 5 6]
    )
  ]

spec :: Spec
spec = do
  describe "Parses different raw string formats of action" $
    forM_ smpls $ \(dotRaw, brackRaw, brackPlusRaw, act) ->
      it (show act) $ do
        parseDotRaw dotRaw `shouldSatisfy` (resRes >>> (== Just act))
        parseBrackRaw brackRaw `shouldSatisfy` (resRes >>> (== Just act))
        parseBrackPlusRaw brackPlusRaw `shouldSatisfy` (resRes >>> (== Just act))
