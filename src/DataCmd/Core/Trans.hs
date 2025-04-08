{- | Transformation type. A conversion with possible failure and logging -}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataCmd.Core.Trans where

import DataCmd.Core.Res

class HasTrans a b where trans :: a -> Res b
