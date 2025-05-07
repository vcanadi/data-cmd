module DataCmd.Raw.NSep where

-- | Group tree nodes with repeated '.' separator
newtype DotRaw = DotRaw { dotRawString :: String } deriving (Eq, Show)

