{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE BangPatterns #-}
module BoringPackrat2 (
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
  Terminal'(..)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Word (Word8)
import qualified Data.Map as Map
import Data.List as L
import Data.Int
import qualified Data.Word8 as W
import qualified Data.Vector as V
import qualified Data.Set as Set
import Prelude as P
import BoringPackrat.Bits
import Data.Maybe
import Debug.Trace

data PEG
  = Sequence [PEG]
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

data Terminal'
  = Lit Int32
  | LitWord [Word8]
  | LitBS BString
  | Range (Int32,Int32)
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
-- n char X n rule = result
--
-- word: duck
-- rules: A , B , C , D
-- (memo = [A, B, C, D], char = d, next = (
--    memo = [A, B, C, D], char = u, next = (
--      memo = [A, B, C, D], char = c, next = (
--        memo = [A, B, C, D] = char = k, next = Void
--        )
--      )
--    )
--
data Layer = Layer {
  charCode :: CharCode,
  memo :: V.Vector Result,
  nextLayer :: Layer
}

-- (char bytes, bytes len, location)
newtype CharCode = CharCode { unCode :: Int32 }
voidCharCode = CharCode 0

isVoidLayer (Layer c _ _) = isVoidCharCode c

voidLayer = Layer voidCharCode undefined voidLayer

isVoidCharCode (CharCode 0) = True
isVoidCharCode _ = False

instance Show Layer where
  show _ = "Layer [...]"

data AST
  = Seq Range [AST]
  | Rule BString Range AST
  | Str Range
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

-- | Returns a substring with range [a,b]
substr :: (Int,Int) -> B.ByteString -> B.ByteString
substr (a,b) = B.take (b - a + 1) . B.drop a

voidParsed a = let !b = a - 1 in Parsed (a,b) Void

parseWord layer loc str = parseWord' str 0 (voidParsed loc layer)
  where
    len = bsLength str
    parseWord' _ _ r@(NoParse _ _) = r
    parseWord' bstr i (Parsed (a,b) ast ly)
      | i >= len = Parsed (a,b) (Str (a,b)) ly
      | otherwise =
        let
          (bpack, consumed) = consumeBits bstr i
          ccode = bytePackToInt bpack
          lycode = unCode $ charCode ly
          ly' = nextLayer ly
          acc' = if ccode == lycode
                   then Parsed (a,b + fromIntegral consumed - 1) ast ly'
                   else NoParse b ["ERR_OO1"]
          !i' = i + 1
          ans = parseWord' bstr i' acc'
        in 
          ans

type PEGRunner = Layer -> Int -> PEG -> Result

locFromRange (_,b) = b + 1

parseSequence :: Layer -> Int -> [PEG] -> PEGRunner -> Result
parseSequence baseLayer baseLoc rules runPeg =
  case ans of
    Right (layer, loc, list) ->
      let range = (baseLoc, loc - 1) in Parsed range (Seq range (reverse list)) layer
    Left err -> err
  where
    ans = foldl' f (Right (baseLayer,baseLoc,[])) rules
    -- use f = map f'
    f (Right (layer,loc,list)) peg =
      case runPeg layer loc peg of
        Parsed range ast layer' -> Right (layer', locFromRange range, ast:list)
        n -> Left n
    f r _ = r

parseChoice :: Layer -> Int -> [PEG] -> PEGRunner -> Result
parseChoice layer loc rules runPeg =
  case find isParsed (map (runPeg layer loc) rules) of
    Just result -> result
    Nothing -> NoParse loc ["ERR_003"]

parseMany :: Layer -> Int -> PEG -> PEGRunner -> Result
parseMany layer loc rule runPeg =
  let
    (asts,layer',loc') = parseMany' [] layer loc rule runPeg
  in
  if null asts
  then voidParsed loc layer'
  else let range = (loc,loc') in Parsed range (Seq range (reverse asts)) layer'

parseMany' asts layer loc rule runPeg =
  case runPeg layer loc rule of
    NoParse _ _ -> (asts,layer,loc)
    Parsed range ast layer' -> parseMany' (ast:asts) layer' (locFromRange range) rule runPeg

parse' :: Grammar -> BString -> (Layer, Map.Map B.ByteString Int)
parse' grammar word =
  let
    --(index,name,peg)
    indexes = L.zipWith (\i (name,rule) -> (i,name,rule)) [0..] grammar

    -- Map RuleName Index
    name2IndexMap = Map.fromList $ fmap (\(i,name,_) -> (name,i)) indexes

    -- Vector RuleNAme
    indexToNameVec = V.fromList $ fmap (\(_,name,_) -> name) indexes

    -- Vector NonTerminal
    nonTerminalsVec
      = V.fromList
      $ fmap
          (\(_,_,peg) -> remapNonTerminals name2IndexMap peg)
          indexes

    -- Step 1
    parsePegNT :: Layer -> Int -> Int -> Result
    parsePegNT = \layer index loc ->
      case nonTerminalsVec V.!? index of
        Just peg' ->
          case parsePEG layer loc peg' of
            Parsed r ast l -> Parsed r (Rule (indexToNameVec V.! index) r ast) l
            NoParse i list ->
              NoParse i (B.concat ["Rule ",indexToNameVec V.! index]:list)
        Nothing -> NoParse index [indexToNameVec V.! index]

    memoNonTerminalFrom layer index = memo layer V.! index

    -- HELLO! Working HERE
    parsePEG layer loc peg =
      case peg of
        Terminal term  ->
          case term of
            LitBS str -> parseWord layer loc str
            _         -> undefined
        Many0 rule     -> parseMany layer loc rule parsePEG
        NT index       -> memoNonTerminalFrom layer index
        Sequence rules -> parseSequence layer loc rules parsePEG
        Choice rules   -> parseChoice layer loc rules parsePEG
        notImplemented -> error $ "Not implemented => " ++ show notImplemented

    generateLayers loc =
      let
        isReachedTheEnd = bsLength word <= loc

        memo' = V.imap (\i _ -> parsePegNT layer i loc) nonTerminalsVec
        (bpack,consumed) = consumeBits word loc
        
        !loc' = loc + fromIntegral consumed

        !code = bytePackToInt bpack 

        charCode' = CharCode code
        
        nextLayer' = generateLayers loc'

        layer = Layer charCode' memo' nextLayer'
      in
        if isReachedTheEnd then voidLayer else layer
  in
    (generateLayers 0, name2IndexMap)

parse :: Grammar -> RuleName -> B.ByteString -> ParsedResult
parse grammar startRulename input =
  let
    (layer, rulename2indexMap) = parse' grammar input
    startIndex = case Map.lookup startRulename rulename2indexMap of
      Just index -> index
      Nothing    -> -1
  in
    case memo layer V.!? startIndex of
      Just k ->
        case k of
          result@(Parsed (_,end) _ _) ->
            if end == B.length input - 1
            then TotallyConsumed result
            else PartiallyConsumed result
          NoParse i' errors -> NotParsed i' errors
      Nothing -> error "Trying to access an empty vector"
