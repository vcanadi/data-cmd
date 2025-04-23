{- | Lexers -}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataCmd.Lexer where

import DataCmd.Core.Tree
import DataCmd.Core.Trans (HasTrans (trans))
import DataCmd.Lexer.NSep (lexNSep)
import DataCmd.Lexer.Brack (lexBrack, lexNormal)
import DataCmd.Core.Res ((#<), (#><))

-- Raw input wrapped into newtypes indicating what type of lexer should be used on it in HasTrans instance


-- | Use NSepLexer with '.' separator
newtype DotLexer = DotLexer { dotLexerRawString :: String }

instance HasTrans DotLexer Tree where
  trans raw = pure (lexNSep '.' $ dotLexerRawString raw) #>< "Dot Lexer"


-- | Use BrackLexer
newtype BrackLexer = BrackLexer { brackLexerRawString :: String }

instance HasTrans BrackLexer Tree where
  trans = lexBrack . brackLexerRawString

-- | Use NormaLexer
newtype NormalLexer = NormalLexer { normalLexerRawString :: String }

instance HasTrans NormalLexer Tree where
  trans = lexNormal . normalLexerRawString
