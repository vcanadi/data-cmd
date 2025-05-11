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
import DataCmd.Core.Res ( (#<), Res, (#<), resNewAN, resNewOR )
import Data.List.NonEmpty ( NonEmpty((:|)), NonEmpty((:|)) )
import Control.Applicative (Alternative(empty))

formerErr :: String -> Res a
formerErr msg = empty #< ("Former error: " <> msg)

treeToForm :: Tree -> Res Form
treeToForm (ND (LF c :| ts)) =  resNewOR ("Former node with Con " <> c ) $ FΣ (FC c) . FΠ <$> traverse (resNewAN "Sel " . treeToForm) ts
treeToForm n@(LF c) = pure (FΣ (FC c) $ FΠ []) #< ("Former leaf " <> show n)
treeToForm _ = formerErr  "Expecting constructor name"

formToTree :: Form -> Tree
formToTree (FPrim v) = LF v
formToTree (c :.. ps) = ND $ LF c :| (formToTree <$> ps)

instance HasTrans Tree Form where
  trnUp t = treeToForm t #< "Tree to Form"
  trnDown t = pure (formToTree t) #< "Form to Tree"
