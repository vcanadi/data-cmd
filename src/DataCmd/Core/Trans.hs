{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Transformation type. A conversion with possible failure and logging
-}

module DataCmd.Core.Trans where

import DataCmd.Core.Res
import Control.Monad ((>=>))

-- | Effectful transformation of 'a' into 'b'
class HasTrans a b where
  trnUp :: a -> Res b
  trnDown :: b -> Res a

-- | Chain of transformations from first type in the type list to the last
class HasTransChain as where
  trnUpChain :: HED as -> Res (LST as)

instance {-# OVERLAPS #-} HasTrans a b => HasTransChain '[a, b] where
  trnUpChain = trnUp @a @b

instance {-# OVERLAPPABLE #-} (HasTrans a b, HasTransChain (b ': as)) => HasTransChain (a ': b ': as) where
  trnUpChain = trnUp @a @b >=> trnUpChain @(b ': as)

-- | List head
type family HED (a :: [k]) :: k where
  HED (a ': as) = a

-- | List tail
type family TIL (a :: [k]) :: [k] where
  TIL (a ': as) = as

-- | List last
type family LST (a :: [k]) :: k where
  LST '[a] = a
  LST (a ': as) = LST as

