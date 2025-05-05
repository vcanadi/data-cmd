{-# LANGUAGE MultiParamTypeClasses #-}
{- | Show tree with brackets
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DataCmd.Raw.Brack.TreeToRaw where

import DataCmd.Tree
import DataCmd.Core.Trans (HasTrans (trans))
import DataCmd.Raw.Brack(BrackLexer(BrackLexer))

showBrack :: Tree -> String
showBrack (LF s) = s
showBrack (ND ts) = concatMap (\t -> "(" <> showBrack t <> ")") ts

instance HasTrans Tree BrackLexer where
  trans = pure . BrackLexer . showBrack

