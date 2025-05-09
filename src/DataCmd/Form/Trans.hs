{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module DataCmd.Form.Trans where
import DataCmd.Form.FormToType
import DataCmd.Core.Trans (HasTrans (trnUp, trnDown))
import DataCmd.Form (Form)
import Control.Arrow ((>>>))
import DataCmd.Core.Res ((#<))
import DataCmd.Form.TypeToForm (HasF(aF), GF)
import GHC.Generics (Generic, Rep)

instance (HasFP a, Generic a, GF (Rep a), Show a) => HasTrans Form a where
  trnUp = aFP @a >>> (#< "Parser")
  trnDown = aF @a >>> (#< "Form render")
