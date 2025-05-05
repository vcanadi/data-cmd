
module DataCmd.Raw.Brack where

-- | Use BrackLexer
newtype BrackLexer = BrackLexer { brackLexerRawString :: String }

-- | Use NormaLexer
newtype NormalLexer = NormalLexer { normalLexerRawString :: String }
