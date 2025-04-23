{- | Result type of various transformations -}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module DataCmd.Core.Res where

import Control.Applicative ((<|>), Alternative (empty))
import DataCmd.Core.Log

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
    Just (Res l1 r1) -> Res (l0 `logSQ` l1) $ r0 >> r1


instance Semigroup a => Semigroup (Res a) where Res l0 r0 <> Res l1 r1 = Res (l0 <> l1) ((<>) <$> r0 <*> r1)
instance Monoid a => Monoid (Res a) where mempty = Res mempty mempty

-- | Append log message to Res
infixl 3 #<
(#<) :: Res a -> String -> Res a
(#<) =  (#*<)

-- | Append AND log message to Res
infixl 3 #*>
(#*>) :: Res a -> String -> Res a
(Res l a) #*> msg = Res (l <> LogLf (Msgs [msg])) a

-- | Prepend AND log message to Res
infixl 3 #*<
(#*<) :: Res a -> String -> Res a
(Res l a) #*< msg = Res (LogLf (Msgs [msg]) <> l) a

-- | Append OR log message to Res
infixl 3 #+>
(#+>) :: Res a -> String -> Res a
(Res l a) #+> msg = Res (l `logOR` LogLf (Msgs [msg])) a

-- | Prepend OR log message to Res
infixl 3 #+<
(#+<) :: Res a -> String -> Res a
(Res l a) #+< msg = Res (LogLf (Msgs [msg]) `logOR` l) a


-- | Prepend SEQ log message to Res
infixl 3 #><
(#><) :: Res a -> String -> Res a
(Res l a) #>< msg = Res (LogLf (Msgs [msg]) `logSQ` l) a

-- | Append SEQ log message to Res
infixl 3 #>>
(#>>) :: Res a -> String -> Res a
(Res l a) #>> msg = Res (l `logSQ` LogLf (Msgs [msg])) a


infixl 3 .#
-- | Create Res with log message
(.#) :: a -> String -> Res a
a .# msg = Res (LogLf (Msgs [msg])) $ Just a

showRes :: Show t => Res t -> String
showRes (Res l r) = showLog l <> "\n" <> show r

