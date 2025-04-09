{- | Result type of various transformations -}
{-# LANGUAGE DeriveFunctor #-}

module DataCmd.Core.Res where

import Control.Applicative ((<|>), Alternative (empty))
import Data.List (intersperse, intercalate)
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy (unpack)

-- | Log tree for hierarchical log structure
data LogT
  = LogAND [LogT]
  | LogOR [LogT]
  | LogSEQ [LogT]
  | LogMsg String
  | LogEmpty
  deriving (Eq)

instance Show LogT where
  show (LogAND ls)  = "AND" <> show ls
  show (LogOR ls)   = "OR" <> show ls
  show (LogSEQ ls)  = "SEQ" <> show ls
  show (LogMsg msg) = msg
  show LogEmpty      = "LogEmpty"

instance Semigroup LogT where
  LogAND ls0 <> LogAND ls1 = LogAND $ ls0 <> ls1
  LogAND ls0 <> l1         = LogAND $ ls0 <> [l1]
  l0         <> LogAND ls1 = LogAND $ l0 : ls1
  LogEmpty   <> l          = l
  l          <> LogEmpty   = l
  l0 <> l1 = LogAND [l0, l1]

instance Monoid LogT where
  mempty = LogEmpty


logOR :: LogT -> LogT -> LogT
LogOR ls0 `logOR` LogOR ls1  = LogOR $ ls0 <> ls1
LogOR ls0 `logOR` l1         = LogOR $ ls0 <> [l1]
l0        `logOR` LogOR ls1  = LogOR $ l0 : ls1
LogEmpty  `logOR` l          = l
l         `logOR` LogEmpty   = l
l0        `logOR` l1         = LogOR [l0, l1]

logSEQ :: LogT -> LogT -> LogT
LogSEQ ls0 `logSEQ` LogSEQ ls1 = LogSEQ $ ls0 <> ls1
LogSEQ ls0 `logSEQ` l1         = LogSEQ $ ls0 <> [l1]
l0        `logSEQ` LogSEQ ls1  = LogSEQ $ l0 : ls1
LogEmpty  `logSEQ` l           = l
l         `logSEQ` LogEmpty    = l
l0        `logSEQ` l1          = LogSEQ [l0, l1]


showLog :: LogT -> String
showLog = unpack . pShow
-- showLog = f 0 []
--   where
--     f k pre (LogAND ls)  = sp pre <> "AND\n"
--                         <> sp pre <> "[\n"
--                         <> sp pre <> intercalate "\n," (f (succ k) ((show k <> "+") : pre) <$> ls) <> "\n"
--                         <> sp pre <> "]"
--     f k pre (LogOR ls)   = sp pre <> "OR\n"
--                         <> sp pre <> "[\n"
--                         <> sp pre <> unlines (f (succ k) ((show k <> "*") : pre) <$> ls)
--                         <> sp pre <> "]"
--     f k pre (LogSEQ ls)  = sp pre <> "SEQ\n"
--                         <> sp pre <> "[\n"
--                         <> sp pre <> unlines (f (succ k) ((show k <> ">") : pre) <$> ls)
--                         <> sp pre <> "]"
--     f k pre (LogMsg msg) = sp pre <> show k <> msg
--     f k pre LogEmpty     = sp pre <> show k <> "()"

--     sp pre = concat $ ("\t" <>) <$> reverse pre


-- | Possible result and log
data Res t = Res
  { resLogT :: LogT
  , resRes :: Maybe t
  } deriving (Eq, Show, Functor)

instance Applicative Res where
  pure = Res mempty . Just
  liftA2 f (Res l0 r0) (Res l1 r1) = Res (l0 <> l1) (f <$> r0 <*> r1)

-- | Try A else try B, concat logs
instance Alternative Res where
  empty = Res mempty Nothing
  Res l0 r0 <|> Res l1 r1 = case r0 of
    Just _ -> Res l0  r0
    _ -> Res (l0 `logOR` l1) r1

instance Monad Res where
  Res l0 r0 >>= f = case f <$> r0 of
    Nothing          -> Res l0 Nothing
    Just (Res l1 r1) -> Res (l0 `logSEQ` l1) $ r0 >> r1


instance Semigroup a => Semigroup (Res a) where Res l0 r0 <> Res l1 r1 = Res (l0 <> l1) ((<>) <$> r0 <*> r1)
instance Monoid a => Monoid (Res a) where mempty = Res mempty mempty

-- | Append log message to Res
infixl 3 ##
(##) :: Res a -> String -> Res a
(##) = (#*<)

-- | Append AND log message to Res
infixl 3 #*>
(#*>) :: Res a -> String -> Res a
(Res l a) #*> msg = Res (l <> LogMsg msg) a

-- | Prepend AND log message to Res
infixl 3 #*<
(#*<) :: Res a -> String -> Res a
(Res l a) #*< msg = Res (LogMsg msg <> l) a

-- | Append OR log message to Res
infixl 3 #+>
(#+>) :: Res a -> String -> Res a
(Res l a) #+> msg = Res (l `logOR` LogMsg msg) a

-- | Prepend OR log message to Res
infixl 3 #+<
(#+<) :: Res a -> String -> Res a
(Res l a) #+< msg = Res (LogMsg msg `logOR` l) a


infixl 3 .#
-- | Create Res with log message
(.#) :: a -> String -> Res a
a .# msg = Res (LogMsg msg) $ Just a

showRes :: Show t => Res t -> String
showRes (Res l r) = showLog l <> "\n" <> show r

