{- | Show Tree with multi-depth separator
-}

module DataCmd.Raw.NSep.TreeToRaw where

import DataCmd.Tree
import Data.List (intercalate)

showNSep :: Char -> Tree -> String
showNSep c = f 1
  where
    f _ (LF s) = s
    f k (ND ts) = intercalate (sep k) $ f (succ k) <$> ts

    sep k = replicate k c
