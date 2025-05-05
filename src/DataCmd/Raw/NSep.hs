module DataCmd.Raw.NSep where

-- | Use NSepLexer with '.' separator
newtype DotLexer = DotLexer { dotLexerRawString :: String } deriving (Eq, Show)

