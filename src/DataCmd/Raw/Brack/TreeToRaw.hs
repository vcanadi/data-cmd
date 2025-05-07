{-# LANGUAGE MultiParamTypeClasses #-}
{- | Show tree with brackets
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DataCmd.Raw.Brack.TreeToRaw where

import DataCmd.Tree
import DataCmd.Core.Trans (HasTrans (trans))
import DataCmd.Raw.Brack(BrackRaw(BrackRaw))

showBrack :: Tree -> String
showBrack (LF s) = s
showBrack (ND ts) = concatMap (\t -> "(" <> showBrack t <> ")") ts

instance HasTrans Tree BrackRaw where
  trans = pure . BrackRaw . showBrack

