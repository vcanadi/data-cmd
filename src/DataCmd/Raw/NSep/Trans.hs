{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

{- | * Simple lexer that extracts Tree of tokens from multi separator tree specification
     * Show Tree with multi-depth separator
-}

module DataCmd.Raw.NSep.Trans where

import DataCmd.Core.Trans (HasTrans (trnUp, trnDown))
import DataCmd.Core.Res ((#<))
import DataCmd.Raw.NSep
import DataCmd.Tree
import Data.Char (isSpace)
import Data.List.NonEmpty ( NonEmpty((:|)), toList )
import qualified Data.List.NonEmpty as NE
import Data.List (intercalate)


-- >>> lexNSep '.' "0 . 1 . 20 .. 21 . 300 ... 301 .. 310 ... 311"
-- ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
lexNSep :: Char -> String -> Tree
lexNSep sep = f 1 . filter (not . isSpace)
  where
    f k = (\case s :| [] | hasNoSep s -> LF s; ss -> ND $ fmap (f (succ k)) ss ) . splitOnNSeps sep k

    hasNoSep = (sep `notElem`)

splitOnNSeps :: Char -> Int -> String -> NonEmpty String
splitOnNSeps sep k = f Nothing "" []
  where
    f _ acc accs []     = NE.reverse (reverse acc :| accs)
    f l acc accs (x:xs) =
      case l of
        Nothing | x /= sep            -> f Nothing          (x:acc)                       accs               xs
                | otherwise           -> f (Just 1)         acc                           accs               xs
        Just l' | x == sep            -> f (Just $ succ l') acc                           accs               xs
                | x /= sep && l' /= k -> f Nothing          (x:(replicate l' sep <> acc)) accs               xs
                | otherwise           -> f Nothing          ""                            (reverse acc:accs) (x:xs)

-- | Tree to raw n separated string
showNSep :: Char -> Tree -> String
showNSep c = f 1
  where
    f _ (LF s) = s
    f k (ND ts) = intercalate (sep k) $ f (succ k) <$> toList ts

    sep k = replicate k c

instance HasTrans DotRaw Tree where
  trnUp raw = pure (lexNSep '.' $ dotRawString raw) #< "DotRaw to Tree"
  trnDown = pure . DotRaw . showNSep '.'
