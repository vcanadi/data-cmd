{- | Extract type Form from Tree -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DataCmd.Tree.TreeToForm where

import DataCmd.Core.Res (Res, (#<))
import DataCmd.Tree (Tree (ND, LF))
import DataCmd.Form (F (..), FC (FC), FΠ (FΠ) )
import Control.Applicative (Alternative(empty))
import DataCmd.Core.Trans (HasTrans (trans))

formerErr :: String -> Res a
formerErr msg = empty #< ("Former error: " <> msg)

treeForm :: Tree -> Res F
treeForm (ND []) = formerErr "Not expecting empty node"
treeForm n@(ND (LF c:ts)) =  FΣ (FC c) . FΠ <$> traverse treeForm ts #< ("Former node " <> show n)
treeForm n@(LF c) = pure (FΣ (FC c) $ FΠ []) #< ("Former leaf " <> show n)

treeForm _ = formerErr  "Expecting constructor name"

instance HasTrans Tree F
  where trans t = treeForm t #< "Former"
