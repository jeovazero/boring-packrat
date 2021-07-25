{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString as B
import Data.Word (Word8)
import qualified Data.Map as Map
import Data.List as L
import qualified Data.Word8 as W
import qualified Data.Vector as V
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
  | Terminal Terminal'
  | NonTerminal ByteString
  | NT Int
  deriving (Show)

(#) = Sequence

data Terminal'
  = Lit Word8
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

type RuleName = ByteString
type NonTerminal' = (RuleName,PEG)
type Range = (Int,Int)

data Result = Parsed Range AST Layer | NotParsed deriving (Show)

data Layer = Layer {
  ans :: V.Vector Result,
  char :: Result
} deriving (Show)

data AST = Seq AST AST | R Int AST | Chr ByteString | Void deriving (Show)

nonTerms =
  [ ("Add", Choice [NonTerminal "Mult" # Terminal (Lit W._plus) # NonTerminal "Add", NonTerminal "Mult"])
  , ("Mult", Choice [NonTerminal "Prim" # Terminal (Lit W._asterisk) # NonTerminal "Mult", NonTerminal "Prim"])
  , ("Prim", Choice [Terminal (Lit W._parenleft) # NonTerminal "Add" # Terminal (Lit W._parenright), NonTerminal "Dec"])
  , ("Dec", Terminal Digit)
  ] :: [(RuleName,PEG)]

remapNT name2IndexMap peg =
  case peg of
    Choice pegs ->
      Choice $ fmap (remapNT name2IndexMap) pegs
    NonTerminal nonTermName ->
      case Map.lookup nonTermName name2IndexMap of
        Just index -> NT index
        Nothing -> error "Rule name not Expected"
    Sequence pegA pegB -> Sequence (remapNT name2IndexMap pegA) (remapNT name2IndexMap pegB)
    other -> other

getIndexLayer layer =
  case char layer of
    NotParsed -> -1
    Parsed (a,_) _ layer' -> a

isParsed (Parsed _ _ _) = True
isParsed _ = False

parse :: [NonTerminal'] -> ByteString -> Layer
parse nonTerms word = parse' (0, B.length word)
  where
    indexes = L.zipWith (\i (name,rule) -> (name,rule,i)) [0..] nonTerms
    name2IndexMap = Map.fromList $ fmap (\(name,_,i) -> (name,i)) indexes
    nonTermsVec = V.fromList $ fmap (\(_,rule,_) -> remapNT name2IndexMap rule) indexes
    parsePegNT = \layer index ->
      case nonTermsVec V.!? index of
        Just peg' ->
          case parsePeg layer peg' of
            Parsed r ast l -> Parsed r (R index ast) l
            NotParsed -> NotParsed
        Nothing -> NotParsed
    usePegNT layer index = (ans layer) V.! index
    parsePeg layer peg =
      case char layer of
        NotParsed -> NotParsed
        Parsed (a,_) ast layer' ->
          let
            x = B.index word a
            result = case peg of
              Choice pegs ->
                case L.find isParsed $ fmap (parsePeg layer) pegs of
                  Just parsed -> parsed
                  Nothing -> NotParsed
              Terminal term ->
                case term of
                  Lit w -> if x == w then Parsed (a,a) ast layer' else NotParsed
                  Digit -> if W.isDigit x then Parsed (a,a) ast layer'  else NotParsed
                  _ -> error "not implemented"
              NT index -> usePegNT layer index
              Sequence pegA pegB ->
                case parsePeg layer pegA of
                  Parsed (beg1,end1) ast1 l ->
                    case parsePeg l pegB of
                      Parsed (beg2,end2) ast2 l' -> Parsed (beg1,end2) (Seq ast1 ast2) l'
                      _ -> NotParsed
                  _ -> NotParsed
              _ -> error "not implemented"
          in
            result
    parse' (a,b) = layer
      where
        layer = Layer ans' chr
        ans' = V.imap (\i _ -> parsePegNT layer i) nonTermsVec
        chr = if a < b
              then Parsed (a,a) (Chr $ B.singleton (B.index word a)) (parse' (a + 1,b))
              else NotParsed
  
main = do
  -- let a = "(1+1)+(2*(2+1*(3*5+(9+1*(3*4*(1*3+(2+1*(1+6*(1*(1+(1*(2*3+(1+3*(3+3)))))))))))))))"
  let a = "(1+1)"
  -- let a = "(0+1+2+3)"
  let x = ans $ parse nonTerms a
  case x V.!? 0 of
    Just k ->
      case k of
        Parsed range ast _ -> print ast
        other -> print other
    Nothing -> error "Impossible"