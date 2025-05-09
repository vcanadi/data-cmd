{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module DataCmd where

import DataCmd.Raw.NSep.Trans ()
import DataCmd.Raw.Brack.Trans ()
import DataCmd.Tree.Trans ()
import DataCmd.Form.Trans ()
import DataCmd.Core.Res (Res)
import DataCmd.Core.Trans (trnUpChain, HasTrans)
import DataCmd.Raw.NSep (DotRaw (DotRaw))
import DataCmd.Tree (Tree)
import DataCmd.Form
import DataCmd.Raw.Brack (BrackRaw (BrackRaw), BrackPlusRaw (BrackPlusRaw))

parseDotRaw :: forall a. HasTrans Form a => String -> Res a
parseDotRaw = trnUpChain @'[DotRaw, Tree, Form, a] . DotRaw

parseBrackRaw :: forall a. HasTrans Form a => String -> Res a
parseBrackRaw = trnUpChain @'[BrackRaw, Tree, Form, a] . BrackRaw

parseBrackPlusRaw :: forall a. (HasTrans Form a) =>String -> Res a
parseBrackPlusRaw = trnUpChain @'[BrackPlusRaw, Tree, Form, a] . BrackPlusRaw
