{-# LANGUAGE LambdaCase #-}

{- | Simple lexer that extracts Tree of tokens from multi separator tree specification
-}
module DataCmd.Lexer.NSep where

-- import DataCmd.Lexer
import Data.Char (isSpace)
import DataCmd.Tree
import Control.Arrow ((>>>))


-- >>> lexNSep '.' "0 . 1 . 20 .. 21 . 300 ... 301 .. 310 ... 311"
-- ND [LF "0",LF "1",ND [LF "20",LF "21"],ND [ND [LF "300",LF "301"],ND [LF "310",LF "311"]]]
lexNSep :: Char -> String -> T
lexNSep sep = f 1 . filter (not . isSpace)
  where
    f k = splitOnNSeps sep k
      >>> (\case []     -> undefined -- empty ## "0 len in prod"
                 (c:ts) -> T $ TΣ (TC c) $ TΠ  $ (\s -> if hasNoSep s then TPrim s else f (succ k) s  ) <$> ts
          )

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
