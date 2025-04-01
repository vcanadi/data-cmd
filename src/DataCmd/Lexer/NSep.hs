
module DataCmd.Lexer.NSep where

import DataCmd.Lexer
import Data.Char (isSpace)


-- | Simple lexter that extracts Tree of tokens from multi separator tree specification
-- >>> lexNSep '.' "0 . 1 . 20 .. 21 . 300 ... 301 .. 310 ... 311"
-- ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
lexNSep :: Char -> String -> Tree
lexNSep sep = f 1 . filter (not . isSpace)
  where
    f k = ND . fmap (\s -> if hasNoSep s then LF s else f (succ k) s  ) . splitOnNSeps sep k

    hasNoSep = (sep `notElem`)

splitOnNSeps :: Eq a => a -> Int -> [a] -> [[a]]
splitOnNSeps sep k = f Nothing [] []
  where
    f _ acc accs []                               = reverse (reverse acc : accs)
    f l acc accs (x:xs) =
      case l of
        Nothing | x /= sep            -> f Nothing          (x:acc)                       accs               xs
                | otherwise           -> f (Just 1)         acc                           accs               xs
        Just l' | x == sep            -> f (Just $ succ l') acc                           accs               xs
                | x /= sep && l' /= k -> f Nothing          (x:(replicate l' sep <> acc)) accs               xs
                | otherwise           -> f Nothing          []                            (reverse acc:accs) (x:xs)
