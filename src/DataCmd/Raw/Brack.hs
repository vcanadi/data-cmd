
module DataCmd.Raw.Brack where

-- | Use bracket grouped string
newtype BrackRaw = BrackRaw { brackRawString :: String }

-- | BrackRaw version with additional optimizations
newtype NormalRaw = NormalRaw { normalRawString :: String }
