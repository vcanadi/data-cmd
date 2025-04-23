{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module DataCmd.Core.Log.Gen where

-- newtype Msgs = Msgs [String] deriving (Eq,Show)

-- -- | Type lifted to kind to represent type level node info
-- data NodeKind = NkAND | NkOR | NkSEQ deriving (Eq,Show)

-- -- | Type lifted to kind to represent type tree info
-- data TreeKind = TkNode NodeKind [TreeKind]
--               | TkLeaf


-- type family Concat (as :: [k]) (bs :: [k]) = (cs :: [k]) where
--   Concat '[] as = as
--   Concat (a ': as) bs = a ': Concat as bs

-- type family TkJoin (nk :: NodeKind) (tk0 :: TreeKind) (tk1 :: TreeKind) = (tk2 :: TreeKind)
--   where
--     TkJoin nk tk 'TkLeaf = tk
--     TkJoin nk 'TkLeaf tk = tk

--     TkJoin NkAND ('TkNode NkAND tk0s) ('TkNode NkAND tk1s) = 'TkNode NkAND (Concat tk0s tk1s)
--     TkJoin NkAND ('TkNode NkAND tk0s) tk1                  = 'TkNode NkAND (Concat tk0s '[ tk1 ] )
--     TkJoin NkAND tk0                  ('TkNode NkAND tk1s) = 'TkNode NkAND (Concat '[ tk0 ] tk1s )

--     TkJoin NkOR ('TkNode NkOR tk0s) ('TkNode NkOR tk1s) = 'TkNode NkOR (Concat tk0s tk1s)
--     TkJoin NkOR ('TkNode NkOR tk0s) tk1                 = 'TkNode NkOR (Concat tk0s '[ tk1 ] )
--     TkJoin NkOR tk0                 ('TkNode NkOR tk1s) = 'TkNode NkOR (Concat '[ tk0 ] tk1s )

--     TkJoin NkSEQ ('TkNode NkSEQ tk0s) ('TkNode NkSEQ tk1s) = 'TkNode NkSEQ (Concat tk0s tk1s)
--     TkJoin NkSEQ ('TkNode NkSEQ tk0s) tk1                  = 'TkNode NkSEQ (Concat tk0s '[ tk1 ] )
--     TkJoin NkSEQ tk0                  ('TkNode NkSEQ tk1s) = 'TkNode NkSEQ (Concat '[ tk0 ] tk1s )

--     TkJoin nk tk0s tk1s = 'TkNode nk '[ tk0s , tk1s ]

-- -- data Log (tk :: TreeKind) where
-- --   LogLf  :: Msgs -> Log 'TkLeaf
-- --   LogAND :: Msgs -> [Log tk] -> Log (TkJoin 'NkAND tk0 tk1)
-- --   LogOR  :: Msgs -> [Log tk] -> Log (TkJoin 'NkOR tk0 tk1)
-- --   LogSEQ :: Msgs -> [Log tk] -> Log (TkJoin 'NkSEQ tk0 tk1)

-- -- showLog :: Log tk -> String
-- -- showLog = unlines . f 0
-- --   where
-- --     f :: Int -> Log tk -> [String]
-- --     f k (LogAND (Msgs ms) ls)  = sym nd : ms
-- --                                 <> concatMap ((\lns -> ("\t"<>) <$> lns ) . f (succ k)) ls

-- -- deriving instance Eq (Log nt)

-- -- logJoin :: Log nt0 -> Log nt1 -> Log (NtJoin nt0 nt1)
