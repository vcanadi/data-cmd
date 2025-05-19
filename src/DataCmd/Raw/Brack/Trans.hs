{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | * Lexer that extract Tree by using brackets as branching indicator
     * Show tree with brackets
-}

module DataCmd.Raw.Brack.Trans where

import DataCmd.Core.Trans (HasTrans (trnUp, trnDown))
import DataCmd.Raw.Brack (BrackRaw (BrackRaw), BrackPlusRaw (BrackPlusRaw, brackPlusRawString), brackRawString)
import Control.Arrow ( (>>>), Arrow(first) )
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isSpace)
import Data.Bool (bool)
import DataCmd.Core.Res
import DataCmd.Tree
import Control.Applicative (Alternative(empty))
import Data.List.NonEmpty (NonEmpty, singleton)

-- | Variant of lexBrack that adds brackets to whitespace separation
-- e.g. sp A (0 0)
lexBrackPlus :: String -> Res Tree
lexBrackPlus = lexBrack . addBrackets

-- | Wrap low level tokens with brackets to interpret things like
-- 'f x (y x)' as '(f)(x)((y)(z))'
addBrackets :: String -> String
addBrackets = filter (not . isSpace) . unwords . fmap (\w -> if needsWrap w then wrap w else w) . words
  where
    isToken w = notElem '(' w && notElem ')' w
    needsWrap w = isToken w || w/="(" && "(" `isPrefixOf` w || w /=")" && ")" `isSuffixOf` w
    wrap w = "(" <> w <> ")"

-- | Extract nested list (tree) of tokens
-- e.g. (sp)(A)((0)(0))
lexBrack :: String -> Res Tree
lexBrack s = if isToken s then pure $ LF s else fmap ND $ traverse lexBrack =<< group s
  where
    group :: String -> Res (NonEmpty String)
    group s = (fstGroup s #< ("First group in " <> s)) >>= \(fg, rest) -> (if null rest then pure (singleton fg) else (singleton fg <>) <$> group rest)
      where
        fstGroup :: String -> Res (String, String)
        fstGroup ('(':cs) = close 0 cs
          where
            close :: Int -> String -> Res (String, String)
            close i (c:s) = if c == ')'
                            then if i== 0 then pure ("", s)
                                          else first (c:) <$> close (pred i) s
                            else first (c:) <$> close (bool i (succ i) (c == '(')) s
            close i _  = empty #< ("Unexpected ending of input; Input: " <> cs <> "; Bracket index: " <> show i)
        fstGroup cs = empty #< ("Group must start with opening bracket '(';  Input: " <> cs)

    isToken w = notElem '(' w && notElem ')' w


-- | Tree to raw bracketed string
showBrack :: Tree -> String
showBrack (LF s) = s
showBrack (ND ts) = concatMap (\t -> "(" <> showBrack t <> ")") ts


instance HasTrans BrackRaw Tree where
  trnUp = (lexBrack . brackRawString) >>> (#< "BrackRaw to Tree")
  trnDown = (pure . BrackRaw . showBrack) >>> (#< "Tree to BrackRaw")

instance HasTrans BrackPlusRaw Tree where
  trnUp = (lexBrackPlus . brackPlusRawString) >>> (#< "BrackPlusRaw to Tree")
  trnDown = (pure . BrackPlusRaw . showT) >>> (#< "Tree to BrackPlusRaw")


