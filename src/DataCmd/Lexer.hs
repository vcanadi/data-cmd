{-# LANGUAGE LambdaCase #-}

{- | 'Lexer' or whatever. Basically, a first step in parsing raw text into some type. Raw string is converted into a intermediate Tree type (tree of tokens/words), which are then parsed by DataCmd.Parser
-}

module DataCmd.Lexer where

import Control.Arrow (Arrow (first))
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isSpace)
import Data.Bool (bool)
import DataCmd.Util

-- | Result of succesful lexing. Intermediate representation for parsing. Raw text is translated into Tree representation (tree of tokens).
data Tree = Leaf String | Node [Tree] deriving (Show, Eq)

isLeaf :: Tree -> Bool
isLeaf = \case (Leaf _) -> True; _ -> False

instance Semigroup Tree where
  Node ts <> Node ts' = Node $ ts <> ts'
  Node [] <> l = l
  Node ts <> l = Node $ ts <> [l]
  l <> Node [] = l
  l <> Node ts = Node $ l:ts
  l <> l' = Node [l,l']

-- | Variant of tBrackLex that adds brackets to whitespace separation
tNormalLex :: String -> Res Tree
tNormalLex = tBrackLex . whiteBracket

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
tBrackLex :: String -> Res Tree
tBrackLex s = if isToken s then Right $ Leaf s else fmap Node $ traverse tBrackLex =<< group s
  where
    group :: String -> Res [String]
    group "" = Right []
    group s = fstGroup s >>= \(fg, rest) -> (fg :) <$> group rest
      where
        fstGroup :: String -> Res (String, String)
        fstGroup ('(':cs) = close 0 cs
          where
            close :: Int -> String -> Res (String, String)
            close i (c:s) = if c == ')'
                            then if i== 0 then Right ("", s)
                                          else first (c:) <$> close (pred i) s
                            else first (c:) <$> close (bool i (succ i) (c == '(')) s
            close i _  = Left [["Unexpected ending of input", "Input: " <> cs, "Bracket index: " <> show i]]
        fstGroup cs = Left [["Group must start with opening bracket '('" , "Input: " <> cs]]

    isToken w = notElem '(' w && notElem ')' w

showT :: Tree -> String
showT (Leaf l) = l
showT (Node ts) = unwords ((bool <$> ((<>")") . ("("<>) . showT) <*> showT <*> isLeaf) <$> ts)

displayT :: Tree -> String
displayT = f 0
  where
    f k (Leaf l) = replicate (2*k) ' ' <> l
    f k (Node ts) = intercalate "\n" $ (\t -> replicate (2*k) ' ' <> sym k <> "\n" <> f (succ k) t)  <$> ts

    sym = (`replicate` '*')
