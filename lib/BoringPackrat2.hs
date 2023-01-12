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
import qualified Data.Map as Map
import Data.List as L
import Data.Int
import qualified Data.Vector as V
import Prelude as P
import BoringPackrat.Bits
import BoringPackrat.Chars as C
import Debug.Trace

data PEG
  = Sequence [PEG]
  | Many0 PEG
  | Many1 PEG
  | ManyN Int PEG
  | Many (Int,Int) PEG
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

voidLayer m = Layer voidCharCode m (voidLayer m)

compareCharCode (Layer c _ _) code = unCode c == code
w8CharCode (Layer c _ _) w8code = fromIntegral $ unCode c

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

parseWord bLayer loc str = parseWord' str 0 (voidParsed loc bLayer)
  where
    len = bsLength str
    parseWord' _ _ r@(NoParse _ _) = r
    parseWord' bstr i (Parsed (a,b) ast layer)
      | i >= len = Parsed (a,b) (Str (a,b)) layer
      | otherwise =
        let
          (bpack, consumed) = consumeBits bstr i
          ccode = bytePackToInt bpack
          ly' = nextLayer layer
          acc' = if compareCharCode layer ccode
                   then Parsed (a,b + fromIntegral consumed) ast ly'
                   else NoParse b ["ERR_OO1"]
          !i' = i + fromIntegral consumed
          ans = parseWord' bstr i' acc'
        in 
          ans

parseTrivialTerm layer loc cond name =
  let
    trivialRng = (loc,loc)
    x = unCode $ charCode layer
    trivialParsed = Parsed trivialRng (Str trivialRng) (nextLayer layer)
  in
    if cond x
    then trivialParsed
    else NoParse loc [name]


parseTerminal :: Layer -> Int -> Terminal' -> Result 
parseTerminal layer loc term =
  let
    trivial = parseTrivialTerm layer loc
  in
  case term of
    LitBS str  -> parseWord layer loc str
    Lit w      -> trivial (w ==) "Lit"
    Digit      -> trivial C.isDigit "Digit"
    HexDigit   -> trivial C.isHexDigit "HexDigit"
    Alpha      -> trivial C.isAlpha "Alpha"
    AlphaLower -> trivial C.isAlphaLower "Alpha"
    AlphaUpper -> trivial C.isAlphaLower "Alpha"
    AlphaDigit -> trivial C.isAlphaLower "Alpha"
    CR         -> trivial (C._cr ==) "Lit_CR"
    LF         -> trivial (C._lf ==) "Lit_LF"
    Dquote     -> trivial (C._quotedbl ==) "Lit_Dquote"
    Tab        -> trivial (C._tab ==) "Lit_Tab"
    SP         -> trivial (C._space ==) "Lit_SP"
    Range (from,to) -> trivial (C.inRange (from,to)) "Lit_Range"
    Specials     -> trivial C.isSpecial "Lit_Special"
    TextSpecials -> trivial C.isTextSpecials "Lit_TextSpecials"

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

parseUntilCond :: Layer -> Int -> PEG -> (Int -> (Bool,Bool)) -> PEGRunner -> Result
parseUntilCond bLayer bLoc rule cond runPeg = parseUntilCond' bLayer bLoc 0 []
  where
    resolve layer loc asts =
      if asts == []
      then voidParsed loc layer
      else 
        let range = (bLoc,loc - 1) in Parsed range (Seq range (reverse asts)) layer
    parseUntilCond' layer loc counter asts =
      let
        (canParse,needParse) = cond counter
      in
      if isVoidLayer layer
      then
        if needParse
        then NoParse loc ["ERR_006"]
        else resolve layer loc asts
      else
        if canParse
        then
          case runPeg layer loc rule of
            Parsed range ast layer' ->
              parseUntilCond' layer' (locFromRange range) (counter + 1) (ast:asts)
            NoParse loc' err ->
              if needParse
              then NoParse loc' (err ++ ["ERR_006"])
              else resolve layer loc' asts
        else resolve layer loc asts

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
      case traceShow ("calling",show peg) peg of
        Terminal term         -> parseTerminal layer loc term
        NT index              -> memoNonTerminalFrom layer index
        Many0 rule            -> parsePEG layer loc (ManyN 0 rule)
        Many1 rule            -> parsePEG layer loc (ManyN 1 rule)
        ManyN n rule          -> parseUntilCond layer loc rule (\i -> (True,i < n)) parsePEG
        Many (minP,maxP) rule -> parseUntilCond layer loc rule (\i -> (i < maxP,i < minP)) parsePEG
        Repeat n rule         -> parsePEG layer loc (Many (n,n) rule)
        Sequence rules        -> parseSequence layer loc rules parsePEG
        Choice rules          -> parseChoice layer loc rules parsePEG
        Optional rule         -> parsePEG layer loc (Many (0,1) rule)
        notImplemented        -> error $ "Not implemented => " ++ show notImplemented

    maxLen = bsLength word
    generateLayers loc =
      let
        !isReachedTheEnd = maxLen <= loc
        voidMemo' = V.imap (\i _ -> parsePegNT (voidLayer voidMemo') i loc) nonTerminalsVec
      in
        if isReachedTheEnd
        then voidLayer voidMemo'
        else
          let 
            memo' = V.imap (\i _ -> parsePegNT layer i loc) nonTerminalsVec
            (bpack,consumed) = consumeBits word loc
            !loc' = loc + fromIntegral consumed
            !code = bytePackToInt bpack 
            charCode' = CharCode code
            nextLayer' = generateLayers loc'
            layer = Layer charCode' memo' nextLayer'
          in
            layer
  in
    (generateLayers 0, name2IndexMap)

parse :: Grammar -> RuleName -> B.ByteString -> ParsedResult
parse grammar startRulename input =
  let
    (layer, rulename2indexMap) = parse' grammar input
    startIndex = case Map.lookup startRulename rulename2indexMap of
      Just index -> index
      Nothing    -> -1
    len = (B.length input - 1)
  in
    case memo layer V.!? startIndex of
      Just k ->
        case k of
          result@(Parsed (_,end) _ _) ->
            if end == len 
            then TotallyConsumed result
            else PartiallyConsumed result
          NoParse i' errors -> NotParsed i' errors
      Nothing -> error "Trying to access an empty vector"
