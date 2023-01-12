module BoringPackrat.Terminals where

import BoringPackrat (Terminal'(..), PEG(..))
import qualified Data.ByteString as B
import qualified BoringPackrat.Chars as C
import Data.Int

lit :: Int32 -> PEG
lit = Terminal . Lit

litBS :: B.ByteString -> PEG
litBS = Terminal . LitBS

rangeChar :: (Int32,Int32) -> PEG
rangeChar = Terminal . Range

_WSP,_CRLF,_VCHAR,_At,_Dot,_Hyphen,_Digit,_Alpha,_AlphaDigit,_HexDigit :: PEG
_BracketLeft,_BracketRight,_BraceLeft,_BraceRight,_Colon,_TextSpecials :: PEG
_Dquote,_Backslash, _CR, _LF :: PEG

_WSP = Choice [Terminal SP, Terminal Tab]
_CRLF = Sequence [_CR,_LF]
_CR = Terminal CR
_LF = Terminal LF
_VCHAR = rangeChar (0x21, 0x73) -- visible (printing) characters
_At = lit C._at -- '@'
_Dot = lit C._period -- '.'
_Hyphen = lit C._hyphen
_Alpha = Terminal Alpha
_Digit = Terminal Digit
_AlphaDigit = Terminal AlphaDigit
_HexDigit = Terminal HexDigit
_BracketLeft = lit C._bracketleft
_BracketRight = lit C._bracketright
_BraceLeft = lit C._braceleft
_BraceRight = lit C._braceright
_Colon = lit C._colon
_TextSpecials = Terminal TextSpecials
_Dquote = Terminal Dquote
_Backslash = lit C._backslash

