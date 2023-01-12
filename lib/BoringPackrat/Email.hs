{-# LANGUAGE OverloadedStrings #-}
module BoringPackrat.Email (emailGrammar,parseEmail,parseEmail',checkEmail,emailAddress) where

import BoringPackrat (
    Terminal'(..),
    PEG(..),
    Grammar,
    ParsedResult,
    parse,
    astFrom,
    AST(..),
    isTotallyConsumed,
    substr
  )
import qualified Data.ByteString.Char8 as B8
import BoringPackrat.Terminals
import Data.Maybe

data Email = Email B8.ByteString B8.ByteString deriving (Show,Eq)

emailAddress :: Email -> B8.ByteString
emailAddress (Email localPart domain) = B8.concat [localPart,"@",domain]

emailFrom :: B8.ByteString -> AST -> Maybe Email
emailFrom input ast =
  case ast of
    Rule "Email" _ (Seq _ [Rule _ rangeL _,_,Rule _ rangeD _]) -> 
        let
          localPart = substr rangeL input 
          domain = substr rangeD input
        in
          Just (Email localPart domain)
    _ -> Nothing

validateLength :: Email -> Bool
validateLength (Email localPart domain) =
  -- localPart "@" domain
  let
    -- The maximum total length of a user name or other local-part is 64
    -- octets.
    localPartLen = B8.length localPart
    localPartCond = localPartLen <= 64

    domainLen = B8.length domain
    -- The maximum total length of a reverse-path or forward-path is 256
    -- octets (including the punctuation and element separators).
    --
    -- Forward-path = Path
    -- Path         = "<" [ A-d-l ":" ] Mailbox ">"
    --
    -- The forward-path will contain at least a pair of angle brackets in
    -- addition to the Mailbox. This limits the Mailbox to 254 characters. 
    pathCond = localPartLen + domainLen + 1 <= 254 
  in
    localPartCond && pathCond

parseEmail :: B8.ByteString -> Maybe Email
parseEmail input =
  let
    result = parseEmail' input
    maybeAst = if isTotallyConsumed result then astFrom result else Nothing
    maybeEmail = maybe Nothing (emailFrom input) maybeAst
    lengthCond = maybe False validateLength maybeEmail
  in
    if lengthCond then maybeEmail else Nothing

checkEmail :: B8.ByteString -> Bool
checkEmail input = isJust $ parseEmail input

parseEmail' :: B8.ByteString -> ParsedResult
parseEmail' = parse emailGrammar "Email"

--
-- https://www.rfc-editor.org/rfc/pdfrfc/rfc5321.txt.pdf
--
-- EMAIL GRAMMAR from RFC 5321
--
--

n,w :: B8.ByteString -> PEG
n = NonTerminal
w = Terminal . LitBS

emailGrammar ::Grammar
emailGrammar
    -- Local-part "@" ( Domain / address-literal )
  = [ ("Email", Sequence [n"LocalPart", _At, Choice [n"Domain", n"AddressLiteral"]])
    -- Dot-string / Quoted-string
  , ("LocalPart", Choice [n"DotString", n"QuotedString"])
    -- sub-domain *("." sub-domain)
  , ("Domain", Sequence[n"Subdomain", Many0 (Sequence[_Dot,n"Subdomain"])])
    -- (ALPHA / DIGIT) [*(ALPHA / DIGIT / "-") (ALPHA / DIGIT)]
    -- or (ALPHA / DIGIT) *(*("-") (ALPHA / DIGIT)) for PEGs
  , ("Subdomain", Sequence[_AlphaDigit, Many0 (Sequence[Many0 _Hyphen, _AlphaDigit])])
  , ("AddressLiteral"
    , Sequence [_BracketLeft
               ,Choice [n"IPV4AddessLiteral", n"IPV6AddressLiteral", n"GeneralAddresLiteral"]
               ,_BracketRight
               ]
    )
    -- Snum 3("."  Snum)
  , ("IPV4AddessLiteral", Sequence[n"Snum",Repeat 3 (n"Snum")])
    -- "IPv6:" IPv6-addr
  , ("IPV6AddressLiteral", Sequence[w"IPv6:", n"IPv6Addr"])
    -- Standardized-tag ":" 1*dcontent
  , ("GeneralAddresLiteral", Sequence[n"StandardizedTag", _Colon, Many1 (n"Dcontent")])
    -- 0*(ALPHA / DIGIT / "-") (ALPHA / DIGIT)
  , ("StandardizedTag", Many1 (Sequence[Many0 _Hyphen, _AlphaDigit]))
    -- Printable US-ASCII, excl. "[", "\", "]"
  , ("Dcontent", Choice [rangeChar (33,90), rangeChar (94,126)])
    -- 1*3DIGIT
  , ("Snum", Many (1,3) _Digit)
    -- IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
  , ("IPv6Addr", Choice [n"IPv6Full", n"IPv6Comp", n"IPv6v4Full", n"IPv6v4Comp"])
    -- 1*4HEXDIG
  , ("IPv6Hex", Many (1,4) _HexDigit)
    -- IPv6-hex 7(":" IPv6-hex)
  , ("IPv6Full", Sequence[n"IPv6Hex", Repeat 7 (Sequence[_Colon, n"IPv6Hex"])])
    -- [IPv6-hex *5(":" IPv6-hex)] "::" [IPv6-hex *5(":" IPv6-hex)]
    -- The "::" represents at least 2 16-bit groups of
    -- zeros.  No more than 6 groups in addition to the
    -- "::" may be present.
  , ("IPv6Comp"
      , Sequence[Optional (Sequence[n"IPv6Hex", Many (0, 5) (Sequence[_Colon,n"IPv6Hex"])])
                ,w"::"
                ,Optional (Sequence[n"IPv6Hex", Many (0, 5) (Sequence[_Colon, n"IPv6Hex"])])
                ]
    )
  -- IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
  , ("IPv6v4Full"
    , Sequence[n"IPv6Hex"
              , Repeat 5 (Sequence[_Colon, n"IPv6Hex"])
              , _Colon
              , n"IPV4AddessLiteral"
              ]
    )
    -- Atom *("."  Atom)
  , ("DotString",Sequence[n"Atom", Many0 (Sequence [_Dot, n"Atom"])])
    -- [IPv6-hex *3(":" IPv6-hex)] "::" [IPv6-hex *3(":" IPv6-hex) ":"] IPv4-address-literal
    -- The "::" represents at least 2 16-bit groups of
    -- zeros.  No more than 4 groups in addition to the
    -- "::" and IPv4-address-literal may be present.
  , ("IPv6v4Comp"
    , Sequence[Optional (Sequence[n"IPv6Hex", Many (0, 3) (Sequence[_Colon, n"IPv6Hex"])])
              , w"::"
              , Optional (Sequence[n"IPv6Hex", Many (0, 3) (Sequence[_Colon, n"IPv6Hex"]), _Colon])
              , n"IPV4AddessLiteral"
              ]
    )
    -- 1*atext
  , ("Atom", Many1 (n"Atext"))
    -- [CFWS] dot-atom-text [CFWS]
  , ("DotAtom"
    , Sequence[Optional (n"CFWS")
              ,n"DotAtomText"
              ,Optional (n"CFWS")
              ]
    )
    -- 1*atext *("." 1*atext)
  , ("DotAtomText", Sequence[Many1 (n"Atext"), Many0 (Sequence[_Dot, Many1 (n"Atext")])])
  , ("Atext", Choice [_AlphaDigit, _TextSpecials])
    -- DQUOTE *QcontentSMTP DQUOTE
  , ("QuotedString"
    , Sequence[_Dquote
              ,Many0 (n"QcontentSMTP")
              ,_Dquote
              ]
    )
    -- qtextSMTP / quoted-pairSMTP
  , ("QcontentSMTP", Choice [n"QtextSMTP", n"QuotedPairSMTP"])
    -- backslash followed by any ASCII
    -- graphic (including itself) or SPace
  , ("QuotedPairSMTP", Sequence[_Backslash, rangeChar (32, 126)])

    -- within a quoted string, any
    -- ASCII graphic or space is permitted
    -- without blackslash-quoting except
    -- double-quote and the backslash itself. 
  , ("QtextSMTP", Choice [rangeChar (32,33), rangeChar (35,91), rangeChar (93, 126)])
    -- Printable ascii, not '\' '"'
  , ("Qtext", Choice [lit 33, rangeChar (35, 91), rangeChar (93, 126)])
    -- "\" (VCHAR / WSP)
  , ("QuotedPair", Sequence[_Backslash, Choice [_VCHAR,_WSP]])
    -- [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
  , ("DomainLiteral"
    , Sequence[ Optional (n"CFWS")
              , _BracketLeft
              , Many0 (Sequence[Optional (n"FWS"), n"Dtext"])
              , Optional (n"FWS")
              , _BracketRight
              , Optional (n"CFWS")
              ]
    )
    -- Printable ASCII chars, not '[' ']' '\'
  , ("Dtext", Choice [rangeChar (33,90), rangeChar (94,126)])
    --  (1*([FWS] comment) [FWS]) / FWS
  , ("CFWS", Choice [Sequence[Many1 (Sequence[Optional (n"FWS"), n"Comment"]), Optional (n"FWS")], n"FWS"])
    -- [*WSP CRLF] 1*WSP
  , ("FWS", Sequence[Optional (Sequence[Many0 _WSP,_CRLF]), Many1 _WSP])
    -- "(" *([FWS] ccontent) [FWS] ")"
  , ("Comment"
    , Sequence[_BracketLeft
              ,Many0 (Sequence[Optional (n"FWS"),n"Ccontent"])
              ,Optional (n"FWS")
              ,_BraceRight
              ]
    )
    -- ctext / quoted-pair / comment
  , ("Ccontent", Choice [n"Ctext", n"QuotedPair", n"Comment"])
    -- printable ascii, not '(' ')' '\'
  , ("Ctext", Choice [rangeChar (33,39), rangeChar (42, 91), rangeChar (93, 126)])
  ] 
