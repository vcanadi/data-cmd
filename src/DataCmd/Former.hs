{- | Extract type form from tree -}

module DataCmd.Former where
import DataCmd.Core.Res (Res, (##))
import DataCmd.Lexer.Tree (Tree (ND, LF))
import DataCmd.Former.Form (F (..), FC (FC), FΣ (FΣ), FΠ (FΠ) )
import Control.Applicative (Alternative(empty))

formerErr :: String -> Res a
formerErr msg = empty ## ("Former error: " <> msg)

treeForm :: Tree -> Res F
treeForm (ND []) = formerErr "Not expecting empty node"
treeForm (ND (LF c:ts)) = F . FΣ (FC c) . FΠ <$> traverse treeForm ts
treeForm (LF c) = pure $ FPrim c
treeForm _ = formerErr  "Expecting constructor name"
