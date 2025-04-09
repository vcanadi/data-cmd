{- | Extract type Form from Tree -}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataCmd.Former where
import DataCmd.Core.Res (Res, (##))
import DataCmd.Lexer.Tree (Tree (ND, LF))
import DataCmd.Former.Form (F (..), FC (FC), FΠ (FΠ) )
import Control.Applicative (Alternative(empty))
import DataCmd.Core.Trans (HasTrans (trans))

formerErr :: String -> Res a
formerErr msg = empty ## ("Former error: " <> msg)

treeForm :: Tree -> Res F
treeForm (ND []) = formerErr "Not expecting empty node"
treeForm (ND (LF c:ts)) =  FΣ (FC c) . FΠ <$> traverse treeForm ts
treeForm (LF c) = pure $ FΣ (FC c) $ FΠ []
treeForm _ = formerErr  "Expecting constructor name"

instance HasTrans Tree F
  where trans = treeForm
