{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{- | 'Lexer' or whatever. Basically, a first step in parsing raw text into some type. Raw string is converted into a intermediate Tree type (tree of tokens/words), which are then parsed by DataCmd.Parser
-}

module DataCmd.Lexer where

import Control.Arrow (Arrow (first), left)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Function ((&))
import Data.Char (isSpace)
import Data.Bool (bool)

-- | Parser trace. Vertically hold trace for each constructor, horizontally for each field of some constructor
type PTrace = [[String]]

showTrace :: PTrace -> String
showTrace = unlines . fmap show

showRes :: Show a => Res a -> String
showRes = either showTrace show

-- | Result type for lexer/parser. Error message trace or result
type Res t = Either PTrace t

-- | Try A else try B, concat error messages on both failures
--
resOR :: Semigroup a => Either a b -> Either a b -> Either a b
a `resOR` b = a & either (\ms0 -> b & left (ms0 <>)) Right

-- Lexer
--
-- | Result of succesful lexing. Intermediate representation for parsing. Raw text is translated into Tree representation (tree of tokens).
data Tree = Leaf String | Node [Tree] deriving (Show)

isLeaf :: Tree -> Bool
isLeaf = \case (Leaf _) -> True; _ -> False

instance Semigroup Tree where
  Node ts <> Node ts' = Node $ ts <> ts'
  Node [] <> l = l
  Node ts <> l = Node $ ts <> [l]
  l <> Node [] = l
  l <> Node ts = Node $ l:ts
  l <> l' = Node [l,l']

-- | Simple lexter that extracts Tree of tokens from multi dot tree specification (note that dots need to be separated by space)
-- >>> tDotLex "0 . 1 . 20 .. 21 . 300 ... 301 .. 310 ... 311"
-- Node [Leaf "0",Leaf "1",Node [Leaf "20",Leaf "21"],Node [Node [Leaf "300",Leaf "301"],Node [Leaf "310",Leaf "311"]]]
tDotLex :: String -> Tree
tDotLex = f 1
  where
    f k = Node . fmap (\s -> if isNested k s then f (succ k) s else Leaf s ) . separate k

    isNested k s = replicate k '.' `isInfixOf` s

    separate :: Int -> String -> [String]
    separate k = filter (not . null) . fmap (unwords . words) . splitOn  (" " <> replicate k '.' <> " ")

    splitOn :: Eq a => [a] -> [a] -> [[a]]
    splitOn xs = f' []
      where
        f' acc []                      = reverse acc
        f' acc ys | xs `isPrefixOf` ys = f' (xs : acc) (drop (length xs) ys)
                 | otherwise           = f' acc (tail ys)


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
