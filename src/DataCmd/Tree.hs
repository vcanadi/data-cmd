{-# LANGUAGE LambdaCase #-}

{- | Tree type (tree of tokens/words), which are then parsed by DataCmd.Parser
-}

module DataCmd.Tree where

import Data.List (intercalate)
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty , singleton, toList)
import GHC.Generics (Generic)

-- | Result of succesful lexing. Intermediate representation for parsing. Raw text is translated into Tree representation (tree of tokens).
data Tree = LF String | ND (NonEmpty Tree) deriving (Show, Eq, Generic)

isLF :: Tree -> Bool
isLF = \case (LF _) -> True; _ -> False

instance Semigroup Tree where
  ND ts <> ND ts' = ND $ ts <> ts'
  ND ts <> l = ND $ ts <> singleton l
  l <> ND ts = ND $ singleton l <> ts
  -- l <> l' = ND [l,l']

showT :: Tree -> String
showT (LF l) = l
showT (ND ts) = unwords ((bool <$> ((<>")") . ("("<>) . showT) <*> showT <*> isLF) <$> toList ts)

displayT :: Tree -> String
displayT = f 0
  where
    f k (LF l) = replicate (2*k) ' ' <> l
    f k (ND ts) = intercalate "\n" $ (\t -> replicate (2*k) ' ' <> sym k <> "\n" <> f (succ k) t)  <$> toList ts

    sym = (`replicate` '*')
