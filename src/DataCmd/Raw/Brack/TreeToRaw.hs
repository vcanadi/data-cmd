{- | Show tree with brackets
-}

module DataCmd.Raw.Brack.TreeToRaw where

import DataCmd.Tree

showBrack :: Tree -> String
showBrack (LF s) = s
showBrack (ND ts) = concatMap (\t -> "(" <> showBrack t <> ")") ts

