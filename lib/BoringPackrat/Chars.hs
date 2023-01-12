module BoringPackrat.Chars where

import Data.Int
import Data.Set as Set

_space :: Int32
_space       = 0x20
_exclam      = 0x21
_quotedbl    = 0x22
_numbersign  = 0x23
_dollar      = 0x24
_percent     = 0x25
_ampersand   = 0x26
_quotesingle = 0x27
_parenleft   = 0x28
_parenright  = 0x29
_asterisk    = 0x2a
_plus        = 0x2b
_comma       = 0x2c
_hyphen      = 0x2d
_period      = 0x2e
_slash       = 0x2f
_bracketleft  = 0x5b
_backslash    = 0x5c
_bracketright = 0x5d
_circum       = 0x5e
_underscore   = 0x5f
_grave        = 0x60
_braceleft  = 0x7b
_bar        = 0x7c
_braceright = 0x7d
_tilde      = 0x7e
_del        = 0x7f
_colon      = 0x3a
_semicolon  = 0x3b
_less       = 0x3c
_equal      = 0x3d
_greater    = 0x3e
_question   = 0x3f
_at         = 0x40

_tab, _lf, _cr :: Int32
_tab = 0x09
_lf  = 0x0a
_cr  = 0x0d



_n0 = 0x30
_n9 = 0x39
_a = 0x61
_A = 0x41
_f = 0x66
_F = 0x46
_z = 0x7a
_Z = 0x5a

specials, textSpecials :: [Int32]
specials
  = [ _parenleft    -- '('
    , _parenright   -- ')'
    , _less         -- '<'
    , _greater      -- '>'
    , _bracketleft  -- '['
    , _bracketright -- ']'
    , _colon        -- ':'
    , _semicolon    -- ';'
    , _at           -- '@'
    , _backslash    -- '\\'
    , _comma        -- ','
    , _period       -- '.'
    , _quotedbl     -- '"'
    ]

textSpecials
  = [ _exclam      -- '!'
    , _numbersign  -- '#'
    , _dollar      -- '$'
    , _percent     -- '%'
    , _ampersand   -- '&'
    , _quotesingle -- '\''
    , _asterisk    -- '*'
    , _plus        -- '+'
    , _hyphen      -- '-'
    , _slash       -- '/'
    , _equal       -- '='
    , _question    -- '?'
    , _circum      -- '^'
    , _underscore  -- '_'
    , _grave       -- '`'
    , _braceleft   -- '{'
    , _braceright  -- '}'
    , _bar         -- '|'
    , _tilde       -- '~'
    ]

specialsSet, textSpecialsSet :: Set.Set Int32 
specialsSet = Set.fromList specials
textSpecialsSet = Set.fromList textSpecials


--}
--
inRange (a,b) n = n >= a && n <= b
isAlpha, isAlphaLower, isAlphaUpper, isDigit, isHexDigit, isAlphaDigit, isSpecial, isTextSpecials :: (Int32 -> Bool)
isAlpha a = isAlphaLower a || isAlphaUpper a
isAlphaLower = inRange (_a,_z)
isAlphaUpper = inRange (_A,_Z)
isDigit = inRange (_n0,_n9)
isHexDigit a = isDigit a || inRange (_a,_f) a || inRange (_A,_F) a
isAlphaDigit a = isDigit a || isAlpha a
isSpecial a = Set.member a specialsSet
isTextSpecials a = Set.member a textSpecialsSet

