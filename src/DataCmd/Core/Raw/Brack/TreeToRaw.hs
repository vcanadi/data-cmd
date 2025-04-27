{- | Show tree with brackets
-}

module DataCmd.Core.Raw.Brack.TreeToRaw where

import DataCmd.Core.Tree

showBrack :: Tree -> String
showBrack (LF s) = s
showBrack (ND ts) = concatMap (\t -> "(" <> showBrack t <> ")") ts

