{-# LANGUAGE OverloadedStrings #-}
module BoringPackrat.EmailGrammar (emailGrammar) where

import BoringPackrat (Terminal'(..), PEG(..), (#), RuleName)
import qualified Data.ByteString as B
import BoringPackrat.Terminals

n,w :: B.ByteString -> PEG
n = NonTerminal
w = Terminal . LitBS

--
-- https://www.rfc-editor.org/rfc/pdfrfc/rfc5321.txt.pdf
--
-- EMAIL SPEC
--
--

emailGrammar :: [(RuleName,PEG)]
emailGrammar
    -- Local-part "@" ( Domain / address-literal )
  = [ ("Email", Sequence [n"LocalPart", _At, Choice [n"Domain", n"AddressLiteral"]])
    -- Dot-string / Quoted-string
  , ("LocalPart", Choice [n"DotString", n"QuotedString"])
    -- sub-domain *("." sub-domain)
  , ("Domain", n"Subdomain" # Many0 (_Dot # n"Subdomain"))
    -- (ALPHA / DIGIT) [*(ALPHA / DIGIT / "-") (ALPHA / DIGIT)]
    -- or (ALPHA / DIGIT) *(*("-") (ALPHA / DIGIT)) for PEGs
  , ("Subdomain", _AlphaDigit # Many0 (Many0 _Hyphen # _AlphaDigit))
  , ("AddressLiteral"
    , _BracketLeft
    # Choice [n"IPV4AddessLiteral", n"IPV6AddressLiteral", n"GeneralAddresLiteral"]
    # _BracketRight
    )
    -- Snum 3("."  Snum)
  , ("IPV4AddessLiteral", n"Snum" # Repeat 3 (n"Snum"))
    -- "IPv6:" IPv6-addr
  , ("IPV6AddressLiteral", w"IPv6:" # n"IPv6Addr")
    -- Standardized-tag ":" 1*dcontent
  , ("GeneralAddresLiteral", n"StandardizedTag" # _Colon # Many1 (n"Dcontent"))
    -- 0*(ALPHA / DIGIT / "-") (ALPHA / DIGIT)
  , ("StandardizedTag", Many1 (Many0 _Hyphen # _AlphaDigit))
    -- Printable US-ASCII, excl. "[", "\", "]"
  , ("Dcontent", Choice [range (33,90), range (94,126)])
    -- 1*3DIGIT
  , ("Snum", Many (1,3) _Digit)
    -- IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
  , ("IPv6Addr", Choice [n"IPv6Full", n"IPv6Comp", n"IPv6v4Full", n"IPv6v4Comp"])
    -- 1*4HEXDIG
  , ("IPv6Hex", Many (1,4) _HexDigit)
    -- IPv6-hex 7(":" IPv6-hex)
  , ("IPv6Full", n"IPv6Hex" # Repeat 7 (_Colon # n"IPv6Hex"))
    -- [IPv6-hex *5(":" IPv6-hex)] "::" [IPv6-hex *5(":" IPv6-hex)]
    -- The "::" represents at least 2 16-bit groups of
    -- zeros.  No more than 6 groups in addition to the
    -- "::" may be present.
  , ("IPv6Comp"
      , Optional (n"IPv6Hex" # Many (0, 5) (_Colon # n"IPv6Hex"))
      # w"::"
      # Optional (n"IPv6Hex" # Many (0, 5) (_Colon # n"IPv6Hex"))
    )
  -- IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
  , ("IPv6v4Full"
    , n"IPv6Hex" # Repeat 5 (_Colon # n"IPv6Hex")
    # _Colon
    # n"IPV4AddessLiteral"
    )
    -- Atom *("."  Atom)
  , ("DotString", n"Atom" # Many0 (_Dot # n"Atom"))
    -- [IPv6-hex *3(":" IPv6-hex)] "::" [IPv6-hex *3(":" IPv6-hex) ":"] IPv4-address-literal
    -- The "::" represents at least 2 16-bit groups of
    -- zeros.  No more than 4 groups in addition to the
    -- "::" and IPv4-address-literal may be present.
  , ("IPv6v4Comp"
    , Optional (n"IPv6Hex" # Many (0, 3) (_Colon # n"IPv6Hex"))
    # w"::"
    # Optional (n"IPv6Hex" # Many (0, 3) (_Colon # n"IPv6Hex") # _Colon)
    # n"IPV4AddessLiteral"
    )
    -- 1*atext
  , ("Atom", Many1 (n"Atext"))
    -- [CFWS] dot-atom-text [CFWS]
  , ("DotAtom"
    , Optional (n"CFWS")
    # n"DotAtomText"
    # Optional (n"CFWS")
    )
    -- 1*atext *("." 1*atext)
  , ("DotAtomText", Many1 (n"Atext") # Many0 (_Dot # Many1 (n"Atext")))
  , ("Atext", Choice [_AlphaDigit, _TextSpecials])
    -- DQUOTE *QcontentSMTP DQUOTE
  , ("QuotedString"
    , _Dquote
    # Many0 (n"QcontentSMTP")
    # _Dquote
    )
    -- qtextSMTP / quoted-pairSMTP
  , ("QcontentSMTP", Choice [n"QtextSMTP", n"QuotedPairSMTP"])
    -- backslash followed by any ASCII
    -- graphic (including itself) or SPace
  , ("QuotedPairSMTP", _Backslash # range (32, 126))

    -- within a quoted string, any
    -- ASCII graphic or space is permitted
    -- without blackslash-quoting except
    -- double-quote and the backslash itself. 
  , ("QtextSMTP", Choice [range (32,33), range (35,91), range (93, 126)])
    -- Printable ascii, not '\' '"'
  , ("Qtext", Choice [lit 33, range (35, 91), range (93, 126)])
    -- "\" (VCHAR / WSP)
  , ("QuotedPair", _Backslash # Choice [_VCHAR,_WSP])
    -- [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
  , ("DomainLiteral"
    , Optional (n"CFWS")
    # _BracketLeft
    # Many0 (Optional (n"FWS") # n"Dtext")
    # Optional (n"FWS")
    # _BracketRight
    # Optional (n"CFWS")
    )
    -- Printable ASCII chars, not '[' ']' '\'
  , ("Dtext", Choice [range (33,90), range(94,126)])
    --  (1*([FWS] comment) [FWS]) / FWS
  , ("CFWS", Choice [Many1 (Optional (n"FWS") # n"Comment") # Optional (n"FWS"), n"FWS"])
    -- [*WSP CRLF] 1*WSP
  , ("FWS", Optional (Many0 _WSP # _CRLF) # Many1 _WSP)
    -- "(" *([FWS] ccontent) [FWS] ")"
  , ("Comment"
    , _BracketLeft
    # Many0 (Optional (n"FWS") # n"Ccontent")
    # Optional (n"FWS")
    # _BraceRight
    )
    -- ctext / quoted-pair / comment
  , ("Ccontent", Choice [n"Ctext", n"QuotedPair", n"Comment"])
    -- printable ascii, not '(' ')' '\'
  , ("Ctext", Choice [range (33,39), range(42, 91), range (93, 126)])
  ] 
