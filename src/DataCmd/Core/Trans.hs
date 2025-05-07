{- | Transformation type. A conversion with possible failure and logging -}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataCmd.Core.Trans where

import DataCmd.Core.Res

-- | Effectful transformation of 'a' into 'b'
class HasTrans a b where trn :: a -> Res b
