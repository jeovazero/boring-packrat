{-# LANGUAGE OverloadedStrings #-}
module BoringPEG.EmailSpec (_Email, _Subdomain, _LocalPart, _Domain) where

import BoringPEG 
import Data.ByteString as B
import qualified Data.Word8 as W

--
-- https://www.rfc-editor.org/rfc/pdfrfc/rfc5321.txt.pdf
--
-- EMAIL SPEC
--

-- Local-part "@" ( Domain / address-literal )
_Email = _LocalPart # _At # Choice [_Domain, _AddressLiteral]

-- Dot-string / Quoted-string
_LocalPart = Choice [_DotString, _QuotedString]

-- sub-domain *("." sub-domain)
_Domain = _Subdomain # Many0 (_Dot # _Subdomain)

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
_DotString = _Atom # Many1 (_Dot # _Atom)

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
_DotAtomText = Many1 _Atext # Many0 (_Dot # Many1 _Atext)

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


