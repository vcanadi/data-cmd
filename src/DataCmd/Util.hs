module DataCmd.Util where

import Data.Function ((&))
import Control.Arrow (ArrowChoice(left))

-- | Parser trace. Vertically hold trace for each constructor, horizontally for each field of some constructor
type TraceLog = [[String]]

showTrace :: TraceLog -> String
showTrace = unlines . fmap show

-- | Result type for lexer/parser. Error message trace or result
type Res t = Either TraceLog t

-- | Try A else try B, concat error messages on both failures
--
resOR :: Semigroup a => Either a b -> Either a b -> Either a b
a `resOR` b = a & either (\ms0 -> b & left (ms0 <>)) Right

showRes :: Show a => Res a -> String
showRes = either showTrace show


