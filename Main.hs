{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString as B
import Data.Word (Word8)
import Data.List as L
import Data.Char as C
import Data.Set as Set
import qualified Data.Word8 as W
import Debug.Trace (trace, traceShowId)

data Foo
  = Sequence Foo Foo
  | Many0 Foo
  | Many1 Foo
  | Many (Int,Int) Foo
  | Repeat Int Foo -- alias for `Many (n, n) Foo`
  | Choice [Foo]
  | Optional Foo
  | And Foo
  | Not Foo
  | Lit Word8
  | LitWord [Word8]
  | Range (Word8,Word8)
  | Dot
  | At
  | Alpha
  | Digit
  | HexDigit
  | AlphaDigit
  | Specials
  | TextSpecials
  | CR
  | LF
  | Dquote
  | Tab
  | SP -- space
  deriving (Show)

-- HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

type Parse a = (a, [Word8])
(#) = Sequence

_WSP = Choice [SP, Tab]
_CRLF = CR # LF
_VCHAR = Range (0x21, 0x73) -- visible (printing) characters

specials
  = [ W._braceleft  -- '('
    , W._braceright -- ')'
    , W._less       -- '<'
    , W._greater    -- '>'
    , W._bracketleft -- '['
    , W._bracketright -- ']'
    , W._colon  -- ':'
    , W._semicolon -- ';'
    , W._at     -- '@'
    , W._backslash  -- '\\'
    , W._comma  -- ','
    , W._period -- '.'
    , W._quotedbl  -- '"'
    ]

text_specials
  = [ W._exclam      -- '!'
    , W._numbersign  -- '#'
    , W._dollar      -- '$'
    , W._percent     -- '%'
    , W._ampersand   -- '&'
    , W._quotesingle -- '\''
    , W._asterisk    -- '*'
    , W._plus        -- '+'
    , W._hyphen      -- '-'
    , W._slash       -- '/'
    , W._equal       -- '='
    , W._question    -- '?'
    , W._circum      -- '^'
    , W._underscore  -- '_'
    , W._grave       -- '`'
    , W._braceleft   -- '{'
    , W._braceright  -- '}'
    , W._bar         -- '|'
    , W._tilde       -- '~'
    ]

textSpecialsSet = Set.fromList text_specials
specialsSet = Set.fromList specials

--
-- https://www.rfc-editor.org/rfc/pdfrfc/rfc5321.txt.pdf
--
-- EMAIL SPEC
--

-- Local-part "@" ( Domain / address-literal )
_Email = _LocalPart # At # Choice [_Domain, _AddressLiteral]

-- Dot-string / Quoted-string
_LocalPart = Choice [_DotString, _QuotedString]

-- sub-domain *("." sub-domain)
_Domain = _Subdomain # Many0 (Dot # _Subdomain)

-- (ALPHA / DIGIT) [*(ALPHA / DIGIT / "-") (ALPHA / DIGIT)]
-- or (ALPHA / DIGIT) *(*("-") (ALPHA / DIGIT)) for PEGs
_Subdomain
  = AlphaDigit
  # Many0 (Many0 (Lit W._hyphen) # AlphaDigit)

_AddressLiteral
  = Lit (W._bracketleft)
  # Choice [_IPV4AddessLiteral, _IPV6AddressLiteral, _GeneralAddresLiteral]
  # Lit (W._bracketright)

-- Snum 3("."  Snum)
_IPV4AddessLiteral = _Snum # Repeat 3 _Snum

-- "IPv6:" IPv6-addr
_IPV6AddressLiteral = LitWord (B.unpack "IPv6:") # _IPv6Addr

-- Standardized-tag ":" 1*dcontent
_GeneralAddresLiteral = _StandardizedTag # Lit W._colon # Many1 _Dcontent

-- *(ALPHA / DIGIT / "-") (ALPHA / DIGIT)
_StandardizedTag = Many1 (Many0 (Lit W._hyphen) # AlphaDigit)

-- Printable US-ASCII, excl. "[", "\", "]"
_Dcontent = Choice [Range (33,90), Range (94,126)]

-- 1*3DIGIT
_Snum = Many (1,3) Digit

-- IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
_IPv6Addr = Choice [_IPv6Full, _IPv6Comp, _IPv6v4Full, _IPv6v4Comp]

-- 1*4HEXDIG
_IPv6Hex = Many (1,4) HexDigit

-- IPv6-hex 7(":" IPv6-hex)
_IPv6Full = _IPv6Hex # Repeat 7 (Lit W._colon # _IPv6Hex)

-- [IPv6-hex *5(":" IPv6-hex)] "::" [IPv6-hex *5(":" IPv6-hex)]
-- The "::" represents at least 2 16-bit groups of
-- zeros.  No more than 6 groups in addition to the
-- "::" may be present.
_IPv6Comp
  = Optional (_IPv6Hex # Many (0, 5) (Lit W._colon # _IPv6Hex))
  # LitWord (B.unpack "::")
  # Optional (_IPv6Hex # Many (0, 5) (Lit W._colon # _IPv6Hex))

-- IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
_IPv6v4Full
  = _IPv6Hex # Repeat 5 (Lit W._colon # _IPv6Hex)
  # Lit W._colon
  # _IPV4AddessLiteral

-- Atom *("."  Atom)
_DotString = _Atom # Many1 (Dot # _Atom)

-- [IPv6-hex *3(":" IPv6-hex)] "::" [IPv6-hex *3(":" IPv6-hex) ":"] IPv4-address-literal
-- The "::" represents at least 2 16-bit groups of
-- zeros.  No more than 4 groups in addition to the
-- "::" and IPv4-address-literal may be present.
_IPv6v4Comp
  = Optional (_IPv6Hex # Many (0, 3) (Lit W._colon # _IPv6Hex))
  # LitWord (B.unpack "::")
  # Optional (_IPv6Hex # Many (0, 3) (Lit W._colon # _IPv6Hex))

-- 1*atext
_Atom = Many1 (_Atext)

-- [CFWS] dot-atom-text [CFWS]
_DotAtom
  = Optional _CFWS
  # _DotAtomText
  # Optional _CFWS

-- 1*atext *("." 1*atext)
_DotAtomText = Many1 _Atext # Many0 (Dot # Many1 _Atext)

_Atext = Choice [AlphaDigit, TextSpecials]

-- DQUOTE *QcontentSMTP DQUOTE
_QuotedString
  = Dquote
  # Many0 _QcontentSMTP
  # Dquote

-- qtextSMTP / quoted-pairSMTP
_QcontentSMTP = Choice [_QtextSMTP, _QuotedPairSMTP]

-- backslash followed by any ASCII
-- graphic (including itself) or SPace
_QuotedPairSMTP = Lit W._backslash # Range (32, 126)

-- within a quoted string, any
-- ASCII graphic or space is permitted
-- without blackslash-quoting except
-- double-quote and the backslash itself. 
_QtextSMTP = Choice [Range (32,33), Range (35,91), Range (93, 126)]

-- Printable ascii, not '\' '"'
_Qtext = Choice [Lit 33, Range (35, 91), Range (93, 126)]

-- "\" (VCHAR / WSP)
_QuotedPair = Lit W._backslash # Choice [_VCHAR,_WSP]

-- [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
_DomainLiteral
  = Optional _CFWS
  # Lit (W._bracketleft)
  # Many0 (Optional _FWS # _Dtext)
  # Optional _FWS
  # Lit (W._bracketright)
  # Optional _CFWS

-- Printable ASCII chars, not '[' ']' '\'
_Dtext = Choice [Range (33,90), Range(94,126)]

--  (1*([FWS] comment) [FWS]) / FWS
_CFWS = Choice [Many1 (Optional _FWS # _Comment) # Optional _FWS, _FWS]

-- [*WSP CRLF] 1*WSP
_FWS = Optional (Many0 _WSP # _CRLF) # Many1 _WSP

-- "(" *([FWS] ccontent) [FWS] ")"
_Comment
  = Lit W._braceleft
  # Many0 (Optional _FWS # _Ccontent)
  # Optional _FWS
  # Lit W._braceright

-- ctext / quoted-pair / comment
_Ccontent = Choice [_Ctext, _QuotedPair, _Comment]

-- printable ascii, not '(' ')' '\'
_Ctext = Choice [Range (33,39), Range(42, 91), Range (93, 126)]

spanFoo :: Foo -> ([Foo], [Word8]) -> ([Foo], [Word8])
spanFoo _ (a,[]) = (L.reverse a, [])
spanFoo foo (a,xs) = case decodeFoo (foo, xs) of
        Left _ -> (a,xs)
        Right (f,w) -> spanFoo foo (f:a,w)

alternativeFoo :: [Foo] -> [Word8] -> Maybe (Foo, [Word8])
alternativeFoo [] words = Nothing
alternativeFoo (foo:xs) words = case decodeFoo (foo, words) of
    Left _ -> alternativeFoo xs words
    Right (f,w) -> Just (f, w)

haveAtLeastOne [] = False
haveAtLeastOne (x:xs) = True

decodeFoo :: Parse Foo -> Either String (Parse Foo)
decodeFoo (foo, []) =
  case foo of
    Many0 f -> Right (Many0 f, [])
    Optional f -> Right (Optional f, [])
    _ -> Left "Empty list"
decodeFoo (foo, words@(x:xs)) =
  case foo of
    Sequence f1 f2 -> do
      (g1, xs') <- decodeFoo (f1,words)
      (g2, ys) <- decodeFoo (f2,xs')
      Right (Sequence g1 g2, ys)
    Many0 f1 ->
      let (result, ys) = spanFoo f1 ([], words) in
      Right (Many0 f1, ys)
    Many1 f1 ->
      let (result, ys) = spanFoo f1 ([], words) in
      if haveAtLeastOne result
      then Right (Many1 f1, ys)
      else Left $ "FMany1 " ++ show (f1, words)
    At ->
      if x == W._at
      then Right (At, xs)
      else Left ("FAt " ++ show [C.chr $ fromIntegral x])
    Dot ->
      if x == W._period -- '.'
      then Right (Dot, xs)
      else Left ("FDot" ++ show [C.chr $ fromIntegral x])
    Alpha ->
      if W.isAlpha x
      then Right (Alpha, xs)
      else Left ("FAlpha " ++ show [C.chr $ fromIntegral x])
    Digit ->
      if W.isNumber x
      then Right (Digit, xs)
      else Left ("FDigit " ++ show [C.chr $ fromIntegral x])
    AlphaDigit ->
      if W.isAlphaNum x
      then Right (AlphaDigit, xs)
      else Left ("FAlphaDigit " ++ show [C.chr $ fromIntegral x])
    Specials ->
      if Set.member x specialsSet
      then Right (Specials, xs)
      else Left ("Specials " ++ show [C.chr $ fromIntegral x])
    Choice flist ->
      case alternativeFoo flist words of
        Just result -> Right result
        Nothing -> Left ("FHyphen " ++ show [C.chr $ fromIntegral x])
    Optional f1  ->
      case decodeFoo (f1,words) of
        Left e -> Right (Optional f1, words)
        Right (f,w) -> Right (Optional f, w)
    And f1 ->
      case decodeFoo (f1, words) of
        Left e -> Left "FAnd "
        Right (f,_) -> Right (f, words)
    Lit w ->
      if x == w
      then Right (Lit w, xs)
      else Left ("Lit " ++ show [C.chr $ fromIntegral w])
    Range (a,b) ->
      if x >= a && x <= b
      then Right (Range (a,b), xs)
      else Left ("Range " ++ show [C.chr $ fromIntegral a, C.chr $ fromIntegral b])
    TextSpecials ->
      if Set.member x textSpecialsSet
      then Right (TextSpecials, xs)
      else Left ("TextSpecials " ++ show [C.chr $ fromIntegral x])
    _ ->
      Left (show "foo")
_FDumbEmail
  = Many1 Alpha -- +[a-zA-Z]
  # At           -- @
  # Many1 Alpha -- +[a-zA-Z]
  # Dot          -- .
  # Many1 Alpha -- +[a-zA-Z]

-- FMany0, FOption and FMany1 doesn't work

main = do
  print
    $ fmap (\(x,y) -> (pack y))
    $ decodeFoo (_Email, unpack "foo-ze-ze@ze-.com")


{- foo-ze-ze.
  [a-z]+(-[a-z]+)*

  &(-.)
-}