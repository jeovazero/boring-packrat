{-# LANGUAGE OverloadedStrings #-}
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
  = Parsed Range AST (Maybe Layer)
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
  memo :: V.Vector Result,
  char :: BytePack,
  loc :: (Int, Word8), -- (startIndex,consumed bits)
  next :: Maybe Layer
}

instance Show Layer where
  show _ = "Layer [...]"

data AST
  = Seq Range [AST]
  | Rule BString Range AST
  | Str BString
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

voidParsed a = Parsed (a,a - 1) Void

parseWord :: BString -> Layer -> Result
parseWord bstr l = parseWord' l 0 (voidParsed a (Just l))
  where
    a = fst . loc $ l
    ln = bsLength bstr
    parseWord' _ _ r@(NoParse _ _) = r
    parseWord' layer i acc@(Parsed (r,_) _ _)
      | i >= ln = acc
      | otherwise =
        let
          (value,consumed) = consumeBits bstr i
          s' = fst . loc $ layer
          c = bytePackToInt . char $ layer
          layer' = next layer
          acc' = if c == bytePackToInt value
              then Parsed (r,s' + fromIntegral consumed - 1) Void layer'
              else NoParse r ["parseWord", B8.pack $ show c]
          i' = i + fromIntegral consumed
        in case layer' of
             Nothing -> acc'
             Just l' -> parseWord' l' i' acc'

parseSequence :: [PEG] -> Layer -> (Layer -> PEG -> Result) -> Result
parseSequence rules layer runPeg = parseSequence' rules (Just layer) rg []
  where
    rg0 = fst . loc $ layer
    rg = (rg0, rg0 - 1)
    parseSequence' [] l rg' acc = Parsed rg' (Seq rg' $ reverse acc) l 
    parseSequence' (_:_) Nothing (u,_) _ = NoParse u ["sequence"]
    parseSequence' (r:rs) (Just l) (u,_) acc =
      case runPeg l r of
        Parsed (_,v') ast l' -> parseSequence' rs l' (u,v') (ast:acc)
        NoParse i e -> NoParse i ("sequence":e)
        
parse' :: Grammar -> BString -> (Maybe Layer, Map.Map B.ByteString Int)
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
    parsePegNT = \layer index ->
      case nonTerminalsVec V.!? index of
        Just peg' ->
          case parsePEG layer peg' of
            Parsed r ast l -> Parsed r (Rule (indexToNameVec V.! index) r ast) l
            NoParse i list ->
              NoParse i (B.concat ["Rule ",indexToNameVec V.! index]:list)
        Nothing -> NoParse index [indexToNameVec V.! index]

    memoNonTerminalFrom layer index = memo layer V.! index

    -- HELLO! Working HERE
    parsePEG layer peg =
      let
        x = bytePackToInt . char $ layer
        (a,_) = loc layer
      in
      case peg of
        Terminal term ->
          case term of
            LitBS ws -> parseWord ws layer
            _ -> undefined
        NT index -> memoNonTerminalFrom layer index
        Sequence rules -> parseSequence rules layer parsePEG
        notImplemented ->
          error $ "Not implemented => " ++ show notImplemented

    generateLayers index =
      let
        inTheVoid = bsLength word <= index
        -- todo: review the parsePegNT, 'cause nonTerminalsVec
        memo' = V.imap (\i _ -> parsePegNT layer i) nonTerminalsVec
        (bpack,consumed) = consumeBits word index
        char' = bpack
        index' = index + fromIntegral consumed
        next' = generateLayers index'
        layer = Layer memo' char' (index, consumed) next'
      in
        if inTheVoid
        then Nothing
        else Just layer
  in
    (generateLayers 0, name2IndexMap)

parse :: Grammar -> RuleName -> B.ByteString -> ParsedResult
parse grammar startRulename input =
  let
    (r, rulename2indexMap) = parse' grammar input
    startIndex = case Map.lookup startRulename rulename2indexMap of
      Just index -> index
      Nothing    -> -1
  in
  case r of
    Just parsedResult ->
      case memo parsedResult V.!? startIndex of
        Just k ->
          case k of
            result@(Parsed (_,end) _ _) ->
              if end == B.length input - 1
              then TotallyConsumed result
              else PartiallyConsumed result
            NoParse i' errors -> NotParsed i' errors
        Nothing -> error "Trying to access an empty vector"

    _ -> error $ show r
