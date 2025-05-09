{- | Extract type Form from Tree -}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DataCmd.Tree.Trans where
import DataCmd.Form
    ( pattern (:..), pattern FPrim, Form(..), FC(FC), FΠ(FΠ) )
import DataCmd.Tree ( Tree(ND, LF), Tree(ND, LF) )
import DataCmd.Core.Trans
    ( HasTrans(trnUp, trnDown) )
import DataCmd.Core.Res ( (#<), Res, (#<) )
import Data.List.NonEmpty ( NonEmpty((:|)), NonEmpty((:|)) )
import Control.Applicative (Alternative(empty))

formerErr :: String -> Res a
formerErr msg = empty #< ("Former error: " <> msg)

treeForm :: Tree -> Res Form
treeForm n@(ND (LF c :| ts)) =  FΣ (FC c) . FΠ <$> traverse treeForm ts #< ("Former node " <> show n)
treeForm n@(LF c) = pure (FΣ (FC c) $ FΠ []) #< ("Former leaf " <> show n)

treeForm _ = formerErr  "Expecting constructor name"

formTree :: Form -> Tree
formTree (FPrim v) = LF v
formTree (c :.. ps) = ND $ LF c :| (formTree <$> ps)

instance HasTrans Tree Form where
  trnUp t = treeForm t #< "Tree to Form"
  trnDown t = pure (formTree t) #< "Form to Tree"
