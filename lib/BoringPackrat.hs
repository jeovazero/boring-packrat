{-# LANGUAGE OverloadedStrings #-}
module BoringPackrat (
  AST(..),
  astFrom,
  Grammar,
  GrammarRule,
  isTotallyConsumed,
  isNotParsed,
  isPartiallyConsumed,
  Layer(..),
  parse,
  ParsedResult(..),
  PEG(..),
  Range,
  Result(..),
  RuleName,
  substr,
  Terminal'(..),
  (#)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Word (Word8)
import qualified Data.Map as Map
import Data.List as L
import qualified Data.Word8 as W
import qualified Data.Vector as V
import qualified Data.Set as Set
import Prelude as P

data PEG
  = Cons PEG PEG
  | Sequence [PEG]
  | Many0 PEG
  | Many1 PEG
  | ManyN Int PEG
  | Many (Int,Int) PEG
  | Many' (Int, Maybe Int) PEG
  | Repeat Int PEG -- alias for `Many (n, n) PEG`
  | Choice [PEG]
  | Optional PEG
  | And PEG
  | Not PEG
  | Terminal Terminal'
  | NonTerminal B.ByteString
  | NT Int
  deriving (Show)

(#) :: PEG -> PEG -> PEG
(#) = Cons

data Terminal'
  = Lit Word8
  | LitWord [Word8]
  | LitBS B.ByteString
  | Range (Word8,Word8)
  | Alpha
  | AlphaLower
  | AlphaUpper
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
   
specials, textSpecials :: [Word8]
specials
  = [ W._parenleft    -- '('
    , W._parenright   -- ')'
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

textSpecials
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

specialsSet, textSpecialsSet :: Set.Set Word8
specialsSet = Set.fromList specials
textSpecialsSet = Set.fromList textSpecials

type RuleName = B.ByteString
type GrammarRule = (RuleName,PEG)
type Grammar = [GrammarRule]
type Range = (Int,Int)

data Result
  = Parsed Range AST Layer
  | NoParse Int [B.ByteString]
  deriving (Show)

data ParsedResult
  -- | When the parsing consumes totally the input
  = TotallyConsumed Result               
  -- | When the parsing consumes partially the input
  | PartiallyConsumed Result             
  | NotParsed Int [B.ByteString]
  deriving (Show)

-- for each char in the input has a Layer
data Layer = Layer {
  memo :: V.Vector Result,
  char :: Result
}

instance Show Layer where
  show _ = "Layer [...]"

data AST
  = Cons' Range AST AST
  | Seq Range [AST]
  | Rule Range B.ByteString AST
  | Str B.ByteString
  | Void
  deriving (Show,Eq)

astFrom :: ParsedResult -> Maybe AST
astFrom (PartiallyConsumed (Parsed _ ast _)) = Just ast
astFrom (TotallyConsumed (Parsed _ ast _)) = Just ast
astFrom _ = Nothing

isTotallyConsumed, isPartiallyConsumed, isNotParsed :: ParsedResult -> Bool
isTotallyConsumed (TotallyConsumed _) = True
isTotallyConsumed _ = False

isPartiallyConsumed (PartiallyConsumed _) = True
isPartiallyConsumed _ = False

isNotParsed (NotParsed _ _) = True
isNotParsed _ = False

isVoid :: AST -> Bool
isVoid Void = True
isVoid _ = False

-- Remaping the rules 'NonTerminal BS' to 'NT Int'
remapNonTerminals :: Map.Map B.ByteString Int -> PEG -> PEG
remapNonTerminals name2IndexMap peg =
  let remap = remapNonTerminals name2IndexMap in
  case peg of
    Choice pegs -> Choice $ fmap remap pegs
    NonTerminal nonTermName ->
      case Map.lookup nonTermName name2IndexMap of
        Just index -> NT index
        Nothing -> error $ concat ["Rule name ", show peg ," not Expected"]
    Cons pegA pegB -> Cons (remap pegA) (remap pegB)
    Sequence pegs -> Sequence . fmap remap $ pegs
    Many0 peg' -> Many0 $ remap peg'
    Many1 peg' -> Many1 $ remap peg'
    ManyN int peg' -> ManyN int $ remap peg'
    Many range peg' -> Many range $ remap peg'
    Many' range peg' -> Many' range $ remap peg'
    Repeat int peg' -> Repeat int $ remap peg'
    Optional peg' -> Optional $ remap peg'
    And peg' -> And $ remap peg'
    Not peg' -> Not $ remap peg'
    other -> other

isParsed :: Result -> Bool
isParsed Parsed {} = True
isParsed _ = False

extractErrors :: Result -> [B8.ByteString]
extractErrors (NoParse _ errors) = errors
extractErrors _ = []

resultOrMergeErrors :: [B8.ByteString] -> Result -> Result
resultOrMergeErrors err (NoParse i errors) = NoParse i (errors ++ err)
resultOrMergeErrors _ other = other

-- | a Void indicates that is the end of the input
isVoidLayer :: Layer -> Bool
isVoidLayer layer =
  case char layer of
    Parsed _ Void _ -> True
    _ -> False

-- | Returns a substring with range [a,b]
substr :: (Int,Int) -> B.ByteString -> B.ByteString
substr (a,b) = B.take (b - a + 1) . B.drop a

parse' :: Grammar -> B.ByteString -> (Layer, Map.Map B.ByteString Int)
parse' grammar word = (parseRange (0, B.length word),name2IndexMap)
  where
    indexes = L.zipWith (\i (name,rule) -> (name,rule,i)) [0..] grammar
    name2IndexMap = Map.fromList $ fmap (\(name,_,i) -> (name,i)) indexes
    nonTerminalsVec
      = V.fromList
      $ fmap
          (\(_,rule,_) -> remapNonTerminals name2IndexMap rule)
          indexes
    indexToNameVec = V.fromList $ fmap (\(name,_,_) -> name) indexes

    parsePegNT = \layer index ->
      case nonTerminalsVec V.!? index of
        Just peg' ->
          case parsePEG layer peg' of
            Parsed r ast l -> Parsed r (Rule r (indexToNameVec V.! index) ast) l
            NoParse i list ->
              NoParse i (B.concat ["Rule ",indexToNameVec V.! index]:list)
        Nothing -> NoParse index [indexToNameVec V.! index]

    memoNonTerminalFrom layer index = memo layer V.! index

    parseSequence baseIndex [] layer (asts,finalIndex) =
      let
        ast = Seq (baseIndex,finalIndex) $ P.reverse asts
      in
        Parsed (baseIndex,finalIndex) ast layer
    parseSequence baseIndex (peg:rs) layer (accAst,_) =
      case parsePEG layer peg of
        Parsed (_,b) ast layer' ->
          parseSequence baseIndex rs layer' (ast:accAst,b)
        other -> other

    parseWord [] _ accIndex acc =
      case acc of
        Parsed (_,b) _ layer' ->
          Parsed range (Str $ substr range word) layer'
            where range = (accIndex,b)
        other -> other
    parseWord (w:ws) layer accIndex acc =
      case parsePEG layer (Terminal $ Lit w) of
        result@(Parsed (a,_) _ layer') ->
          parseWord ws layer' (accIndex' a) result
        other -> other
      where
        accIndex' index = if isParsed acc then accIndex else index

    mapManyResult result start acc =
      case result of
        Parsed (_,end) cur layer' ->
            Parsed (start,end) (Seq (start,end) (reverse (cur:acc))) layer'
        r -> r

    repeatPEGwithMax start peg max' count layer acc lastResult
      | count >= max' = (count, mapManyResult lastResult start $ tail acc)
      | otherwise =
          case parsePEG layer peg of
            result@(Parsed _ cur layer') ->
              if isVoidLayer layer'
              then (count + 1, mapManyResult result start acc)
              else repeatPEGwithMax start peg max' (count + 1) layer' (cur:acc) result
            NoParse _ errors -> (count,resultOrMergeErrors errors $ mapManyResult lastResult start $ tail acc)

    repeatPEG start peg count layer acc lastResult =
      case parsePEG layer peg of
        result@(Parsed _ cur layer') ->
          if isVoidLayer layer'
          then (count + 1, mapManyResult result start acc)
          else repeatPEG start peg (count + 1) layer' (cur:acc) result
        NoParse _ errors -> (count,resultOrMergeErrors errors $ mapManyResult lastResult start $ tail acc)

    parsePEG layer peg =
      case char layer of
        Parsed (a,_) ast layer' ->
          case peg of
            Choice pegs ->
              case L.find isParsed $ fmap (parsePEG layer) pegs of
                Just parsed -> parsed
                Nothing -> NoParse a ["Choice"]
            Optional peg' ->
              if isVoid ast
              then Parsed (a,a-1) Void layer
              else
                case parsePEG layer peg' of
                  NoParse _ _ -> Parsed (a,a-1) Void layer
                  result' -> result'
            And peg' ->
              case parsePEG layer peg' of
                Parsed {} -> Parsed (a,a-1) Void layer 
                other -> resultOrMergeErrors ["And"] other
            Not peg' ->
              case parsePEG layer peg' of
                NoParse _ _ -> Parsed (a,a-1) Void layer 
                _ -> NoParse a ["Not"]
            Many' (min', maybeMax) peg' ->
              if isVoid ast
              then
                if min' == 0
                then Parsed (a,a-1) Void layer
                else NoParse a ["Many","Void"]
              else
                case maybeMax of
                  Just max' ->
                    let
                      (count, result) =
                        repeatPEGwithMax a peg' max' 0 layer [] (NoParse a ["Many"])
                    in
                      if min' <= count && count <= max'
                      then result
                      else case result of
                        NoParse i _ -> NoParse i ["Many"]
                        Parsed (_,b) _ _ -> NoParse (b + 1) ["Many"]
                  Nothing ->
                    let
                      (count, result) =
                        repeatPEG a peg' 0 layer [] (NoParse a ["Many"])
                      cursorIndex =
                        case result of
                          NoParse i _ -> i
                          Parsed (_,b) _ _ -> b + 1
                    in 
                      if min' <= count
                      then
                        case result of
                          NoParse _ errors ->
                            if min' == 0
                            then Parsed (a,a-1) Void layer
                            else NoParse (a + count) ("Many":errors)
                          result' -> result'
                      else NoParse cursorIndex (extractErrors result)
            Many0 peg' -> parsePEG layer $ Many' (0, Nothing) peg'
            Many1 peg' -> parsePEG layer $ Many' (1, Nothing) peg'
            ManyN n peg' -> parsePEG layer $ Many' (n, Nothing) peg'
            Repeat n peg' -> parsePEG layer $ Many' (n, Just n) peg'
            Many (min',max') peg' ->
              parsePEG layer $ Many' (min', Just max') peg'
            Terminal term ->
              if isVoid ast then NoParse a ["Terminal","Void"]
              else
                let x = B.index word a in
                case term of
                  Lit w ->
                    if x == w
                    then Parsed (a,a) ast layer'
                    else NoParse a ["Lit"]
                  LitWord ws -> parseWord ws layer a (NoParse a ["LitWord"])
                  LitBS ws ->
                    parsePEG layer $ Terminal $ LitWord $ B.unpack ws
                  Digit ->
                    if W.isDigit x
                    then Parsed (a,a) ast layer'
                    else NoParse a ["Digit"]
                  HexDigit ->
                    if W.isHexDigit x
                    then Parsed (a,a) ast layer'
                    else NoParse a ["HexDigit"]
                  Alpha ->
                    if W.isAlpha x
                    then Parsed (a,a) ast layer'
                    else NoParse a ["Alpha"]
                  AlphaLower ->
                    if W.isLower x
                    then Parsed (a,a) ast layer'
                    else NoParse a ["AlphaLower"]
                  AlphaUpper ->
                    if W.isUpper x
                    then Parsed (a,a) ast layer'
                    else NoParse a ["AlphaUpper"]
                  AlphaDigit ->
                    if W.isAlphaNum x
                    then Parsed (a,a) ast layer'
                    else NoParse a ["AlphaDigit"]
                  CR -> parsePEG layer $ Terminal $ Lit W._cr
                  LF -> parsePEG layer $ Terminal $ Lit W._lf
                  Dquote -> parsePEG layer $ Terminal $ Lit W._quotedbl
                  Tab -> parsePEG layer $ Terminal $ Lit W._tab
                  SP -> parsePEG layer $ Terminal $ Lit W._space
                  Range (from,to) -> 
                    if x >= from && x <= to
                    then Parsed (a,a) ast layer'
                    else NoParse a ["Range"]
                  Specials ->
                    if Set.member x specialsSet
                    then Parsed (a,a) ast layer'
                    else NoParse a ["Specials"]
                  TextSpecials ->
                    if Set.member x textSpecialsSet
                    then Parsed (a,a) ast layer'
                    else NoParse a ["TextSpecials"]
            NT index -> memoNonTerminalFrom layer index
            Cons pegA pegB ->
              case parsePEG layer pegA of
                Parsed (beg1,_) ast1 l ->
                  case parsePEG l pegB of
                    Parsed (_,end2) ast2 l' ->
                      Parsed (beg1,end2) (Cons' (beg1,end2) ast1 ast2) l'
                    other -> other
                other -> other
            Sequence rules -> parseSequence a rules layer ([],a-1)
            notImplemented ->
              error $ "Not implemented => " ++ show notImplemented
        other -> other

    parseRange (a,b) = layer
      where
        layer = Layer memo' chr
        memo' = V.imap (\i _ -> parsePegNT layer i) nonTerminalsVec
        chr | a < b = 
                Parsed (a,a)
                (Str $ B.singleton (B.index word a))
                (parseRange (a + 1,b))
            | otherwise =
                if a == b
                then Parsed (a,a) Void (parseRange (a + 1,b))
                else NoParse a [] 

parse :: Grammar -> RuleName -> B.ByteString -> ParsedResult
parse grammar startRulename input =
  case memo parsedResult V.!? startIndex of
    Just k ->
      case k of
        result@(Parsed (_,end) _ _) ->
          if end == B.length input - 1
          then TotallyConsumed result
          else PartiallyConsumed result
        NoParse i' errors -> NotParsed i' errors
    Nothing -> error "Trying to access an empty vector"
  where
    (parsedResult, rulename2indexMap) = parse' grammar input
    startIndex = case Map.lookup startRulename rulename2indexMap of
      Just index -> index
      Nothing -> -1
   
