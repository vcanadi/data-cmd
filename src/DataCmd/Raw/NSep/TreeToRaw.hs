{-# LANGUAGE MultiParamTypeClasses #-}
{- | Show Tree with multi-depth separator
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DataCmd.Raw.NSep.TreeToRaw where

import DataCmd.Tree
import DataCmd.Raw.NSep
import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import DataCmd.Core.Trans (HasTrans (trn))

showNSep :: Char -> Tree -> String
showNSep c = f 1
  where
    f _ (LF s) = s
    f k (ND ts) = intercalate (sep k) $ f (succ k) <$> toList ts

    sep k = replicate k c

instance HasTrans Tree DotRaw where
  trn = pure . DotRaw . showNSep '.'
