{-# LANGUAGE OverloadedStrings #-}
module BoringPackrat (
  AST(..),
  astFrom,
  Grammar,
  GrammarRule,
  isAllParsed,
  isNotParsed,
  isPartialParsed,
  Layer(..),
  parse,
  ParsedResult(..),
  PEG(..),
  prettyPrint,
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
  = AllParsed Result
  | PartialParsed Result
  | NotParsed Int [B.ByteString]
  deriving (Show)

data Layer = Layer {
  ans :: V.Vector Result,
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
astFrom (PartialParsed (Parsed _ ast _)) = Just ast
astFrom (AllParsed (Parsed _ ast _)) = Just ast
astFrom _ = Nothing

isAllParsed, isPartialParsed, isNotParsed :: ParsedResult -> Bool
isAllParsed (AllParsed _) = True
isAllParsed _ = False

isPartialParsed (PartialParsed _) = True
isPartialParsed _ = False

isNotParsed (NotParsed _ _) = True
isNotParsed _ = False

applyIdent :: Int -> IO ()
applyIdent ident = B8.putStr $ B8.replicate ident ' '

loopList :: B8.ByteString -> B8.ByteString -> Int -> [AST] -> IO ()
loopList input prefix ident (ast:xs) = do
  applyIdent ident
  B8.putStr prefix
  prettyPrint' (ident + 2) True input ast
  loopList input prefix ident xs
loopList _ _ _ [] = pure ()


prettyPrint :: B.ByteString -> AST -> IO ()
prettyPrint = prettyPrint' 0 False

prettyPrint' :: Int -> Bool -> B.ByteString -> AST -> IO ()
prettyPrint' ident ignoreIdent input ast =
  let
    applyIdent' = if ignoreIdent then pure () else applyIdent ident
    padding = if ignoreIdent then 0 else 2
    showInfo range = do
      P.putStr $ show range ++ " -> "
      P.putStr "\""
      B8.putStr $ substr range input
      P.putStrLn "\""
  in
  case ast of
    Cons' range ast1 ast2 -> do
      applyIdent'
      P.putStr "Cons -> "
      showInfo range
      loopList input ":" (ident + padding) [ast1,ast2]
    Seq range asts -> do
      applyIdent'
      P.putStr "Sequence -> "
      showInfo range
      loopList input "-" (ident + padding) asts
    Rule range name ast' -> do
      applyIdent'
      B8.putStr $ B8.concat ["Rule ",name," -> "]
      showInfo range
      prettyPrint' (ident + padding) False input ast'
    Str value -> do
      applyIdent'
      P.putStrLn ("Str " ++ show value)
    Void -> do
      P.putStrLn "*"

isVoid :: AST -> Bool
isVoid Void = True
isVoid _ = False

remapNT :: Map.Map B.ByteString Int -> PEG -> PEG
remapNT name2IndexMap peg =
  let remap = remapNT name2IndexMap in
  case peg of
    Choice pegs -> Choice $ fmap remap pegs
    NonTerminal nonTermName ->
      case Map.lookup nonTermName name2IndexMap of
        Just index -> NT index
        Nothing -> error "Rule name not Expected"
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

mergeErrors :: [B8.ByteString] -> Result -> Result
mergeErrors err (NoParse i errors) = NoParse i (errors ++ err)
mergeErrors _ other = other

isVoidLayer :: Layer -> Bool
isVoidLayer layer =
  case char layer of
    Parsed _ Void _ -> True
    _ -> False

substr :: (Int,Int) -> B.ByteString -> B.ByteString
substr (a,b) = B.take (b - a + 1) . B.drop a

parsePEG :: Grammar -> B.ByteString -> (Layer, Map.Map B.ByteString Int)
parsePEG nonTerms word = (parse' (0, B.length word),name2IndexMap)
  where
    indexes = L.zipWith (\i (name,rule) -> (name,rule,i)) [0..] nonTerms
    name2IndexMap = Map.fromList $ fmap (\(name,_,i) -> (name,i)) indexes
    nonTermsVec =
      V.fromList $ fmap (\(_,rule,_) -> remapNT name2IndexMap rule) indexes
    indexToNameVec = V.fromList $ fmap (\(name,_,_) -> name) indexes
    manyResult start (Parsed (_,end) _ layer) =
      Parsed (start,end) (Str $ substr (start,end) word) layer
    manyResult _ noParse = noParse
    parsePegNT = \layer index ->
      case nonTermsVec V.!? index of
        Just peg' ->
          case parsePeg layer peg' of
            Parsed r ast l -> Parsed r (Rule r (indexToNameVec V.! index) ast) l
            NoParse i list ->
              NoParse i (B.concat ["Rule ",indexToNameVec V.! index]:list)
        Nothing -> NoParse index [indexToNameVec V.! index]
    usePegNT layer index = ans layer V.! index
    parseSequence baseIndex [] layer (asts,finalIndex) =
      let
        ast = Seq (baseIndex,finalIndex) $ P.reverse asts
      in
        Parsed (baseIndex,finalIndex) ast layer
    parseSequence baseIndex (peg:rs) layer (accAst,_) =
      case parsePeg layer peg of
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
      case parsePeg layer (Terminal $ Lit w) of
        result@(Parsed (a,_) _ layer') ->
          parseWord ws layer' (accIndex' a) result
        other -> other
      where
        accIndex' index = if isParsed acc then accIndex else index
    repeatPeg' peg max' count layer lastResult
      | count >= max' = (count, lastResult)
      | otherwise =
          case parsePeg layer peg of
            result@(Parsed _ _ layer') ->
              if isVoidLayer layer'
              then (count + 1, result)
              else repeatPeg' peg max' (count + 1) layer' result
            NoParse _ errors -> (count,mergeErrors errors lastResult)
    repeatPeg peg count layer lastResult =
      case parsePeg layer peg of
        result@(Parsed _ _ layer') ->
          if isVoidLayer layer'
          then (count + 1, result)
          else repeatPeg peg (count + 1) layer' result
        NoParse _ errors -> (count,mergeErrors errors lastResult)
    parsePeg layer peg =
      case char layer of
        Parsed (a,_) ast layer' ->
          case peg of
            Choice pegs ->
              case L.find isParsed $ fmap (parsePeg layer) pegs of
                Just parsed -> parsed
                Nothing -> NoParse a ["Choice"]
            Optional peg' ->
              if isVoid ast
              then Parsed (a,a-1) Void layer
              else
                case parsePeg layer peg' of
                  NoParse _ _ -> Parsed (a,a-1) Void layer
                  result' -> result'
            And peg' ->
              case parsePeg layer peg' of
                Parsed {} -> Parsed (a,a-1) Void layer 
                other -> mergeErrors ["And"] other
            Not peg' ->
              case parsePeg layer peg' of
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
                        repeatPeg' peg' max' 0 layer (NoParse a ["Many"])
                    in
                      if min' <= count && count <= max'
                      then manyResult a result
                      else case manyResult a result of
                        NoParse i _ -> NoParse i ["Many"]
                        Parsed (_,b) _ _ -> NoParse (b + 1) ["Many"]
                  Nothing ->
                    let
                      (count, result) =
                        repeatPeg peg' 0 layer (NoParse a ["Many"])
                      cursorIndex =
                        case result of
                          NoParse i _ -> i
                          Parsed (_,b) _ _ -> b + 1
                    in 
                      if min' <= count
                      then
                        case manyResult a result of
                          NoParse _ errors ->
                            if min' == 0
                            then Parsed (a,a-1) Void layer
                            else NoParse (a + count) ("Many":errors)
                          result' -> result'
                      else NoParse cursorIndex (extractErrors result)
            Many0 peg' -> parsePeg layer $ Many' (0, Nothing) peg'
            Many1 peg' -> parsePeg layer $ Many' (1, Nothing) peg'
            ManyN n peg' -> parsePeg layer $ Many' (n, Nothing) peg'
            Repeat n peg' -> parsePeg layer $ Many' (n, Just n) peg'
            Many (min',max') peg' ->
              parsePeg layer $ Many' (min', Just max') peg'
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
                    parsePeg layer $ Terminal $ LitWord $ B.unpack ws
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
                  AlphaDigit ->
                    if W.isAlphaNum x
                    then Parsed (a,a) ast layer'
                    else NoParse a ["AlphaDigit"]
                  CR -> parsePeg layer $ Terminal $ Lit W._cr
                  LF -> parsePeg layer $ Terminal $ Lit W._lf
                  Dquote -> parsePeg layer $ Terminal $ Lit W._quotedbl
                  Tab -> parsePeg layer $ Terminal $ Lit W._tab
                  SP -> parsePeg layer $ Terminal $ Lit W._space
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
            NT index -> usePegNT layer index
            Cons pegA pegB ->
              case parsePeg layer pegA of
                Parsed (beg1,_) ast1 l ->
                  case parsePeg l pegB of
                    Parsed (_,end2) ast2 l' ->
                      Parsed (beg1,end2) (Cons' (beg1,end2) ast1 ast2) l'
                    other -> other
                other -> other
            Sequence rules -> parseSequence a rules layer ([],a-1)
            notImplemented ->
              error $ "Not implemented => " ++ show notImplemented
        other -> other
    parse' (a,b) = layer
      where
        layer = Layer ans' chr
        ans' = V.imap (\i _ -> parsePegNT layer i) nonTermsVec
        chr | a < b = 
                Parsed (a,a)
                (Str $ B.singleton (B.index word a))
                (parse' (a + 1,b))
            | otherwise =
                if a == b
                then Parsed (a,a) Void (parse' (a + 1,b))
                else NoParse a [] 

parse :: Grammar -> RuleName -> B.ByteString -> ParsedResult
parse nonTerms rulename input =
  case result of
    Just k ->
      case k of
        result''@(Parsed (_,end) _ _) ->
          if end == B.length input - 1
          then AllParsed result''
          else PartialParsed result''
        NoParse i' errors -> NotParsed i' errors
    Nothing -> error "Trying to access an empty vector"
  where
    result = ans result' V.!? i
    i = case Map.lookup rulename name2indexMap of
          Just i' -> i'
          Nothing -> -1
    (result', name2indexMap) = parsePEG nonTerms input
