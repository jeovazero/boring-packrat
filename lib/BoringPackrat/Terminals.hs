module BoringPackrat.Terminals where

import BoringPackrat (Terminal'(..), PEG(..), (#))
import qualified Data.Word8 as W

lit :: W.Word8 -> PEG
lit = Terminal . Lit

range :: (W.Word8,W.Word8) -> PEG
range = Terminal . Range

_WSP,_CRLF,_VCHAR,_At,_Dot,_Hyphen,_Digit,_AlphaDigit,_HexDigit :: PEG
_BracketLeft,_BracketRight,_BraceLeft,_BraceRight,_Colon,_TextSpecials :: PEG
_Dquote,_Backslash :: PEG

_WSP = Choice [Terminal SP, Terminal Tab]
_CRLF = Terminal CR # Terminal LF
_VCHAR = range (0x21, 0x73) -- visible (printing) characters
_At = lit W._at -- '@'
_Dot = lit W._period -- '.'
_Hyphen = lit W._hyphen
_Digit = Terminal Digit
_AlphaDigit = Terminal AlphaDigit
_HexDigit = Terminal HexDigit
_BracketLeft = lit W._bracketleft
_BracketRight = lit W._bracketright
_BraceLeft = lit W._braceleft
_BraceRight = lit W._braceright
_Colon = lit W._colon
_TextSpecials = Terminal TextSpecials
_Dquote = Terminal Dquote
_Backslash = lit W._backslash

