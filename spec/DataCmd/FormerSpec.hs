module DataCmd.FormerSpec where

import Test.Hspec
import DataCmd.Former.Form
import Control.Monad(forM_)
import DataCmd.Lexer.Tree (Tree (ND, LF))
import DataCmd.Former (treeForm)
import DataCmd.Core.Res (Res(resRes))

smplsFormer :: [(String, Tree, F)]
smplsFormer =
  [
   ( "works on simple example"
    , ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [LF "300",ND [LF "310",LF "311"]]]
    , F $ FΣ (FC "0") $ FΠ
      [ FPrim "1"
      , F $ FΣ (FC "20") $ FΠ [FPrim "21"]
      , F $ FΣ (FC "300") $ FΠ
        [ F $ FΣ (FC "310") $ FΠ [FPrim "311"]
        ]
      ]
    )

  , ( "works on  singleton"
    , LF "1"
    , FPrim "1"
    )


  ]

shouldResultIn :: (Show t, Eq t) => Res t -> t -> Expectation
shouldResultIn a b = resRes a `shouldBe` Just b

specFormer :: Spec
specFormer =
  describe "treeForm" $
    forM_ smplsFormer $ \(testDesc, tree, form) ->
      it testDesc $
        treeForm tree  `shouldResultIn` form

spec :: Spec
spec = specFormer
