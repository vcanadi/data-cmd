
module DataCmd.Raw.Brack where

-- | Use bracket grouped string
newtype BrackRaw = BrackRaw { brackRawString :: String } deriving (Eq, Show)

-- | BrackRaw version with additional optimizations
newtype BrackPlusRaw = BrackPlusRaw { brackPlusRawString :: String } deriving (Eq, Show)
