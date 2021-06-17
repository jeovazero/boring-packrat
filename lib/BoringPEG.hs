{-# LANGUAGE OverloadedStrings #-}
module BoringPEG (
  decodePEG,
  PEG(..),
  (#),
  _At,
  _Dot,
  _CRLF,
  _VCHAR,
  _WSP
) where

import Data.ByteString as B
import Data.Word (Word8)
import Data.List as L
import Data.Char as C
import Data.Set as Set
import Prelude as P
import qualified Data.Word8 as W
import Debug.Trace (trace, traceShowId)

data PEG
  = Sequence PEG PEG
  | Many0 PEG
  | Many1 PEG
  | Many (Int,Int) PEG
  | Repeat Int PEG -- alias for `Many (n, n) PEG`
  | Choice [PEG]
  | Optional PEG
  | And PEG
  | Not PEG
  | Lit Word8
  | LitWord [Word8]
  | Range (Word8,Word8)
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

type Parse a = (a, [Word8])
(#) = Sequence

_WSP = Choice [SP, Tab]
_CRLF = CR # LF
_VCHAR = Range (0x21, 0x73) -- visible (printing) characters
_At = Lit W._at -- '@'
_Dot = Lit W._period -- '.'

specials
  = [ W._braceleft    -- '('
    , W._braceright   -- ')'
    , W._less         -- '<'
    , W._greater      -- '>'
    , W._bracketleft  -- '['
    , W._bracketright -- ']'
    , W._colon        -- ':'
    , W._semicolon    -- ';'
    , W._at           -- '@'
    , W._backslash    -- '\\'
    , W._comma        -- ','
    , W._period       -- '.'
    , W._quotedbl     -- '"'
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

spanPEG :: PEG -> ([PEG], [Word8]) -> ([PEG], [Word8])
spanPEG _ (a,[]) = (L.reverse a, [])
spanPEG foo (a,xs) = case decodePEG (foo, xs) of
        Left _ -> (a,xs)
        Right (f,w) -> spanPEG foo (f:a,w)

alternativePEG :: [PEG] -> [Word8] -> Maybe (PEG, [Word8])
alternativePEG [] words = Nothing
alternativePEG (foo:xs) words = case decodePEG (foo, words) of
    Left _ -> alternativePEG xs words
    Right (f,w) -> Just (f, w)

haveAtLeastOne [] = False
haveAtLeastOne (x:xs) = True

decodePEG :: Parse PEG -> Either String (Parse PEG)
decodePEG (foo, []) =
  case foo of
    Many0 f -> Right (Many0 f, [])
    Optional f -> Right (Optional f, [])
    _ -> Left "Empty list"
decodePEG (foo, words@(x:xs)) =
  case foo of
    Sequence f1 f2 -> do
      (g1, xs') <- decodePEG (f1,words)
      (g2, ys) <- decodePEG (f2,xs')
      Right (Sequence g1 g2, ys)
    Many0 f1 ->
      let (result, ys) = spanPEG f1 ([], words) in
      Right (Many0 f1, ys)
    Many1 f1 ->
      let (result, ys) = spanPEG f1 ([], words) in
      if haveAtLeastOne result
      then Right (Many1 f1, ys)
      else Left $ "FMany1 " ++ show (f1, words)
    Many (a,b) f1 ->
      let
        (result, ys) = spanPEG f1 ([], words)
        len = P.length result
      in
      if len >= a && len <= b
      then Right (Many (a,b) f1, ys)
      else Left $ "FMany " ++ show (f1, words)
    Repeat n f1 ->
      decodePEG (Many (n,n) f1, words)
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
      case alternativePEG flist words of
        Just result -> Right result
        Nothing -> Left ("FChoice " ++ show [C.chr $ fromIntegral x])
    Optional f1  ->
      case decodePEG (f1,words) of
        Left e -> Right (Optional f1, words)
        Right (f,w) -> Right (Optional f, w)
    And f1 ->
      case decodePEG (f1, words) of
        Left e -> Left "FAnd "
        Right (f,_) -> Right (f, words)
    Not f1 ->
      case decodePEG (f1, words) of
        Left e -> Right (Not f1, words)
        Right (f,_) -> Left "FNot "
    Lit w ->
      if x == w
      then Right (Lit w, xs)
      else Left ("Lit " ++ show [C.chr $ fromIntegral w])
    LitWord ws ->
      case L.stripPrefix ws words of
        Just rest -> Right (LitWord ws, rest)
        _ -> Left ("LitWords " ++ show [pack ws])
    HexDigit ->
      if W.isHexDigit x
      then Right (TextSpecials, xs)
      else Left ("TextSpecials " ++ show [C.chr $ fromIntegral x])
    CR ->
      decodePEG (Lit W._cr, words)
    LF ->
      decodePEG (Lit W._lf, words)
    Dquote ->
      decodePEG (Lit W._quotedbl, words)
    Tab ->
      decodePEG (Lit W._tab, words)
    SP ->
      decodePEG (Lit W._space, words)
    Range (a,b) ->
      if x >= a && x <= b
      then Right (Range (a,b), xs)
      else Left ("Range " ++ show [C.chr $ fromIntegral a, C.chr $ fromIntegral b])
    TextSpecials ->
      if Set.member x textSpecialsSet
      then Right (TextSpecials, xs)
      else Left ("TextSpecials " ++ show [C.chr $ fromIntegral x])

