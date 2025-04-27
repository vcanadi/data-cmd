{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DataCmd.Core.Tree.FormToTree where
import DataCmd.Core.Form (pattern (:..), F, pattern FPrim)
import DataCmd.Core.Tree (Tree (ND, LF))
import DataCmd.Core.Trans (HasTrans (trans))
import DataCmd.Core.Res ((#<))

formTree :: F -> Tree
formTree (FPrim v) = LF v
formTree (c :.. ps) = ND $ LF c : (formTree <$> ps)

instance HasTrans F Tree
  where trans t = pure (formTree t) #< "Form to Tree"
