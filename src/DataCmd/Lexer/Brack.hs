{- | Lexer that extract Tree by using brackets as branching indicator
-}
{-# LANGUAGE LambdaCase #-}

module DataCmd.Lexer.Brack where

import Control.Arrow (Arrow (first))
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isSpace)
import Data.Bool (bool)
import DataCmd.Util
-- import DataCmd.Lexer
import Control.Applicative (Alternative(empty))
import DataCmd.Tree (T (T, TPrim), TΣ (TΣ), TC (TC), TΠ (TΠ))

-- | Variant of lexBrack that adds brackets to whitespace separation
-- e.g. sp A (0 0)
lexNormal :: String -> Res T
lexNormal = lexBrack . whiteBracket

-- | Wrap low level tokens with brackets to interpret things like
-- 'f x (y x)' as '(f)(x)((y)(z))'
whiteBracket :: String -> String
whiteBracket = filter (not . isSpace) . unwords . fmap (\w -> if needsWrap w then wrap w else w) . words
  where
    isToken w = notElem '(' w && notElem ')' w
    needsWrap w = isToken w || w/="(" && "(" `isPrefixOf` w || w /=")" && ")" `isSuffixOf` w
    wrap w = "(" <> w <> ")"

-- | Extract nested list (tree) of tokens
-- e.g. (sp)(A)((0)(0))
lexBrack :: String -> Res T
lexBrack s = if isToken s then pure $ TPrim s else (\case [] -> undefined; (c:ts) -> T . TΣ (TC c) . TΠ  <$> traverse lexBrack ts) =<< group s
  where
    group :: String -> Res [String]
    group "" = pure []
    group s = fstGroup s >>= \(fg, rest) -> (fg :) <$> group rest
      where
        fstGroup :: String -> Res (String, String)
        fstGroup ('(':cs) = close 0 cs
          where
            close :: Int -> String -> Res (String, String)
            close i (c:s) = if c == ')'
                            then if i== 0 then pure ("", s)
                                          else first (c:) <$> close (pred i) s
                            else first (c:) <$> close (bool i (succ i) (c == '(')) s
            close i _  = empty ## ("Unexpected ending of input; Input: " <> cs <> "; Bracket index: " <> show i)
        fstGroup cs = empty ## ("Group must start with opening bracket '(';  Input: " <> cs)

    isToken w = notElem '(' w && notElem ')' w
