module DataCmd
   ( module DataCmd.Form.FormToType
   , module DataCmd.Form.TypeToForm
   , module DataCmd.Tree.FormToTree
   , module DataCmd.Tree.TreeToForm
   , module DataCmd.Raw.NSep.RawToTree
   , module DataCmd.Raw.NSep.TreeToRaw
   , module DataCmd.Raw.Brack.RawToTree
   , module DataCmd.Raw.Brack.TreeToRaw
   ) where

import DataCmd.Raw.NSep.RawToTree
import DataCmd.Raw.NSep.TreeToRaw
import DataCmd.Raw.Brack.RawToTree
import DataCmd.Raw.Brack.TreeToRaw
import DataCmd.Tree.TreeToForm
import DataCmd.Tree.FormToTree
import DataCmd.Form.FormToType
import DataCmd.Form.TypeToForm
import DataCmd.Core.Res (Res)
import DataCmd.Raw.NSep (DotRaw (DotRaw))
import DataCmd.Tree (Tree)
import DataCmd.Form (F)
import Control.Arrow ((>>>))
import DataCmd.Core.Trans (HasTrans(trn))
import Control.Monad ((>=>))

-- parseDotRaw :: forall a. String -> Res a
-- parseDotRaw = DotRaw
--          >>> trn @DotRaw @Tree
--          >=> trn @Tree @F
--          >=> trn @F @a
