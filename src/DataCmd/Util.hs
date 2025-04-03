{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module DataCmd.Util where

import Data.List (intercalate)

-- | Parser trace. Vertically hold trace for each constructor, horizontally for each field of some constructor
type Entry = [String]

newtype Log = Log { lLog :: [Entry] } deriving (Show, Eq, Semigroup)

newtype ErrLog = ErrLog { elLog :: [Entry] } deriving (Show, Eq, Semigroup)

showLog :: Log -> String
showLog = unlines . fmap (intercalate ";") . lLog

showErrLog :: ErrLog -> String
showErrLog = unlines . fmap (intercalate ";") . elLog

-- | Add log message to Res
(##) :: a -> String -> Res a
a ## msg = ResSucc (Log [[msg]], a)

-- | Create Res with log message
(.#) :: a -> String -> Res a
a .# msg = ResSucc (Log [[msg]], a)

-- | Result type for lexer/parser. Error message trace or result
data Res t
  = ResErr ErrLog
  | ResSucc (Log, t)
  deriving (Eq, Show, Functor)

bimapRes :: (ErrLog -> ErrLog) -> ((Log, t) -> (Log, t)) -> Res t -> Res t
bimapRes errF _    (ResErr l) = ResErr $ errF l
bimapRes _    resF (ResSucc res) = ResSucc $ resF res

-- | Try A else try B, concat error messages on both failures
--
resOR :: Res a -> Res a -> Res a
resOR = \cases
  (ResSucc logRes)       _                      -> ResSucc logRes
  (ResErr err0)          (ResErr err1)          -> ResErr $ err0 <> err1
  (ResErr (ErrLog err0)) (ResSucc (Log l, res)) -> ResSucc (Log $ l <> err0, res)

showRes :: Show a => Res a -> String
showRes (ResErr el)      = showErrLog el
showRes (ResSucc (l, r)) = showLog l <> "\n" <> show r
