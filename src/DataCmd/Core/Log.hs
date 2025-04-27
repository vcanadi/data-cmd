{- | Tree structured log type -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module DataCmd.Core.Log where
import Data.Ord (comparing)

-- | Node type in a log tree
-- EMPTY - no structural info
-- AN - conjunction
-- OR - disjunction
-- SQ - join of two distinct logs
data NdType = NtAN | NtOR | NtSQ deriving (Eq,Show)

-- | Message list stored in a tree node
newtype Msgs = Msgs [String] deriving (Eq, Show, Semigroup, Monoid)


-- | Nice display of log tree node
ndName :: NdType -> String
ndName = \case NtAN -> "AND"; NtOR -> "OR"; NtSQ -> "SEQ"

-- | Nice display of log tree node
ndSym :: NdType -> Char
ndSym = \case NtAN -> '*'; NtOR -> '+'; NtSQ -> '>'


-- | Log tree for hierarchical log structure
data LogT = LogNd NdType Msgs [LogT]
          | LogLf Msgs
  deriving (Eq)

-- | Join two logs with specific node join type. Minimal on-spot normalization is applied
logJoinWith :: NdType -> LogT -> LogT -> LogT
-- | adhoc merging of leaves
-- logJoinWith NtAN (LogNd NtAN ms0 ls0) (LogLf ms1)          = LogNd NtAN (ms0 <> ms1) ls0
-- logJoinWith NtAN (LogLf ms0)          (LogNd NtAN ms1 ls1) = LogNd NtAN (ms0 <> ms1) ls1

-- logJoinWith NtOR (LogNd NtOR ms0 ls0) (LogLf ms1)          = LogNd NtOR (ms0 <> ms1) ls0
-- logJoinWith NtOR (LogLf ms0)          (LogNd NtOR ms1 ls1) = LogNd NtOR (ms0 <> ms1) ls1

-- logJoinWith NtSQ (LogNd NtSQ ms0 ls0) (LogLf ms1)          = LogNd NtSQ (ms0 <> ms1) ls0
-- logJoinWith NtSQ (LogLf ms0)          (LogNd NtSQ ms1 ls1) = LogNd NtSQ (ms0 <> ms1) ls1

-- | adhoc merging based on join node type
-- logJoinWith NtAN (LogNd NtAN ms0 ls0) l1                   = LogNd NtAN ms0 (ls0 <> [l1])
-- logJoinWith NtAN l0                   (LogNd NtAN ms1 ls1) = LogNd NtAN ms1 ([l0] <> ls1)

-- logJoinWith NtOR (LogNd NtOR ms0 ls0) l1                   = LogNd NtOR ms0 (ls0 <> [l1])
-- logJoinWith NtOR l0                   (LogNd NtOR ms1 ls1) = LogNd NtOR ms1 ([l0] <> ls1)

-- logJoinWith NtSQ (LogNd NtSQ ms0 ls0) l1                   = LogNd NtSQ ms0 (ls0 <> [l1])
-- logJoinWith NtSQ l0                   (LogNd NtSQ ms1 ls1) = LogNd NtSQ ms1 ([l0] <> ls1)

-- | adhoc regular flattening of the same node types
-- logJoinWith NtAN (LogNd NtAN ms0 ls0) (LogNd NtAN ms1 ls1) = LogNd NtAN (ms0 <> ms1) (ls0 <> ls1)
-- logJoinWith NtOR (LogNd NtOR ms0 ls0) (LogNd NtOR ms1 ls1) = LogNd NtOR (ms0 <> ms1) (ls0 <> ls1)
-- logJoinWith NtSQ (LogNd NtSQ ms0 ls0) (LogNd NtSQ ms1 ls1) = LogNd NtSQ (ms0 <> ms1) (ls0 <> ls1)

logJoinWith nt    l0                  l1                   = LogNd nt mempty [l0, l1]

-- | Complexity of a log tree
logSize :: LogT -> Int
logSize (LogLf (Msgs ms)) = 1 + length ms
logSize (LogNd _ (Msgs ms) ls) = 1 + length ms + sum (fmap logSize ls)

-- | Compare complexities of two logs
logSizeCompare :: LogT -> LogT -> Ordering
logSizeCompare  = comparing logSize

-- | < operator of logSize
logSizeLT :: LogT -> LogT -> Bool
logSizeLT = ((==LT).) . logSizeCompare

-- TODO: Rework normalization together with LogT model
-- | Log normalization until convergence
-- normalizeLog :: LogT -> LogT
-- normalizeLog l = if normalized `logSizeLT`  l then normalizeLog normalized else l
--   where
--     normalized = normalizeDown $ normalizeUp l

-- -- | Normalize each level from root down
-- normalizeDown :: LogT -> LogT
-- normalizeDown (LogLf ms) = LogLf ms
-- normalizeDown (LogNd nt ms ls) = LogNd nt ms $ fmap normalizeDown $ groupLogLevel ls

-- -- | Normalize each level from leaves up
-- normalizeUp :: LogT -> LogT
-- normalizeUp (LogLf ms) = LogLf ms
-- normalizeUp (LogNd nt ms ls) = LogNd nt ms $ groupLogLevel $ fmap normalizeUp ls


-- -- | Group adjacent same type nodes on one level
-- groupLogLevel :: [LogT] -> [LogT]
-- groupLogLevel [] = []
-- groupLogLevel (l0':ls') = reverse $ uncurry (:) $ foldl' f (l0',[]) ls'
--   where
--     f = \cases
--       (LogLf ms0,lsAcc)               (LogLf ms1)                         -> (LogLf $ ms0 <> ms1,lsAcc)
--       (LogNd nt0 ms0 ls0, lsAcc)      (LogLf (Msgs []))                   -> (LogNd nt0 ms0 ls0, lsAcc)
--       (LogLf (Msgs []), lsAcc)        (LogNd nt1 ms1 ls1)                 -> (LogNd nt1 ms1 ls1, lsAcc)
--       (l0@(LogNd nt0 ms0 ls0), lsAcc) l1@(LogNd nt1 ms1 ls1) | nt0 == nt1 -> (LogNd nt0 (ms0 <> ms1) (ls0 <> ls1), lsAcc)
--                                                              | otherwise  -> (l1, l0:lsAcc)
--       (l0, lsAcc)                     l1                                  -> (l1, l0:lsAcc)


-- | Different ways of joining two logs
logAN, logOR, logSQ :: LogT -> LogT -> LogT
logAN = logJoinWith NtAN
logOR = logJoinWith NtOR
logSQ = logJoinWith NtSQ

emptyLog :: LogT
emptyLog = LogLf mempty

appendMsg :: String -> LogT -> LogT
appendMsg s (LogNd nt ms ls) = LogNd nt (ms <> Msgs [s]) ls
appendMsg s (LogLf (Msgs ms)) = LogLf (Msgs $ ms <> [s])

prependMsg :: String -> LogT -> LogT
prependMsg s (LogNd nt ms ls) = LogNd nt (Msgs [s] <> ms) ls
prependMsg s (LogLf ms) = LogLf $ Msgs [s] <> ms

showLog :: LogT -> String
showLog = unlines . fmap prefixLine . f 0
  where
    f :: Int -> LogT -> [(Int, Maybe Char, String)]
    f _ (LogLf (Msgs ms))       = ind 0 "[LF]" : (ind 1 <$> ms)
    f k (LogNd nd (Msgs ms) ls) = [ind 0 $ "[" <> ndName nd <> "]"]
                               <> (ind 1 <$> ms)
                               <> concatMap ( (\case [] -> []; (l:lns') -> addElem (ndSym nd) l : (addInd <$> lns'))
                                            . ((ind 0 $ ""):) . f (succ k)
                                            ) ls

    ind :: Int -> String -> (Int,Maybe Char,String)
    ind s = (s,Nothing,)

    addInd (s,b,l) = (succ s,b,l)

    addElem sn (s,_,l) = (succ s,Just sn,l)

    prefixLine (s,b,l) = concat (replicate s "    ")
                      <> maybe "    " ((<> "   ") . pure  ) b
                      <> l

printLog :: LogT -> IO ()
printLog = putStrLn . showLog

-- instance Show LogT where show = showLog
deriving instance Show LogT
