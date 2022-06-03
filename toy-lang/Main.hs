{-# LANGUAGE OverloadedStrings #-}
import BoringPackrat (
    parse,
    astFrom,
    AST(..),
    Terminal'(..),
    PEG(..),
    Grammar,
    substr
  )
import BoringPackrat.Terminals
import qualified Data.Word8 as W
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)
import Text.Pretty.Simple (pPrint)
-- import BoringPackrat.PrettyPrint (prettyPrint)
-- import Debug.Trace

n = NonTerminal
_ParenLeft  = Terminal $ Lit W._parenleft  -- '('
_ParenRight = Terminal $ Lit W._parenright -- ')'
_Times      = Terminal $ Lit W._asterisk   -- '*'
_Plus       = Terminal $ Lit W._plus       -- '+'
_Minus      = Terminal $ Lit W._hyphen     -- '-'
_Apostrofe  = Terminal $ Lit W._quotesingle -- "'" 
_Equal      = Terminal $ Lit W._equal
_Lower      = Terminal AlphaLower
_Upper      = Terminal AlphaUpper
_Lf         = Choice [_WSP, _CRLF]

grammar :: Grammar
grammar =
  [ ("program", Sequence [n"funs", n"breakline", n"runcall", n"_lf"])
  , ("funs", Sequence [n"fun", Many0 $ n"brfun"])
  , ("brfun", Sequence [n"breakline", n"fun"])
  , ("runcall", Sequence [n"run", n"__", n"expr"])
  , ("breakline", Sequence [n"_", n"lf", n"_lf"])
  , ("expr", Choice [n"add", n"addend"])
  , ("add", Sequence [n"addend",n"_",n"plusminus",n"_", n"expr"])
  , ("addend", Choice [n"mult",n"factor"])
  , ("mult", Sequence [n"factor",n"_",n"times",n"_", n"addend"])
  , ("factor", Choice [n"parens", n"decimal", n"call"])
  , ("parens", Sequence [_ParenLeft,n"_",n"expr",n"_", _ParenRight])
  , ("fun", Sequence [n"identifier", Many0 $ n"params", n"_", _Equal, n"_", n"expr"])
  , ("params", Sequence [n"__", n"param"])
  , ("param", Choice [n"identifier", n"decimal"])
  , ("call", Sequence [n"identifier", Many0 $ n"args"])
  , ("args", Sequence [n"__", n"argument"])
  , ("argument", Choice [n"identifier", n"parens", n"decimal"])
  , ("identifier", Sequence [Many1 $ n"wW", Many0 _Apostrofe])
  , ("wW", _Alpha)
  , ("W", _Upper)
  , ("w", _Lower)
  , ("decimal", Many1 _Digit)
  , ("equal", _Equal)
  , ("apostrofe", _Apostrofe)
  , ("plusminus", Choice [n"plus",n"minus"])
  , ("plus", _Plus)
  , ("minus", _Minus)
  , ("times", _Times)
  , ("run", litBS "run")
  , ("_", Many0 _WSP)
  , ("__", Many1 _WSP)
  , ("lf", Choice [_CR, _LF, _CRLF])
  , ("_lf", Many0 $ _Lf)
  , ("__lf", Many1 $ _Lf)
  ]

type Identifier = B8.ByteString

data Program    = Program [Fun] RunCall             deriving (Show)
data Fun        = Fun Identifier [Params] Expr      deriving (Show)
data Params     = ParamId Identifier | ParamNum Int deriving (Show)
data RunCall    = RunCall Expr                      deriving (Show)
data Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Decimal Int
    | Call Identifier [Arg] 
    deriving (Show)
data Arg
    = ArgId Identifier
    | ArgExpr Expr
    deriving (Show)

toArg input ast =
  case ast of
    Rule range "identifier" _ -> ArgId $ substr range input
    Rule _ "parens" _         -> ArgExpr $ toExpr input ast
    Rule _ "decimal" _        -> ArgExpr $ toExpr input ast
    rule -> error ("wrong identifier rule: " ++ show rule)

toArgs input ast =
  case ast of
    Seq _ args -> fmap (\(Rule _ "args" (Seq _ [_, Rule _ "argument" arg])) -> toArg input arg) args
    Void -> []
    rule -> error $ "wrong args rule: " ++ show rule

toExpr input ast =
  case ast of
    Rule _ "add" (Seq _ [a,_,Rule _ _ op,_,b]) ->
      case op of
        Rule _ "plus" _ -> Add (toExpr input a) (toExpr input b)
        Rule _ "minus" _ -> Sub (toExpr input a) (toExpr input b)
        rule -> error $ "wrong addend op: " ++ show rule
    Rule _ "mult" (Seq _ [a,_,_,_,b]) ->
      Mult (toExpr input a) (toExpr input b)
    Rule _ "parens" (Seq _ [_,_,e,_,_]) ->
      toExpr input e
    Rule range "decimal" _ ->
      Decimal (read $ B8.unpack $ substr range input)
    Rule _ "call" (Seq _ [Rule range "identifier" _, args]) ->
      Call (substr range input) (toArgs input args)
    Rule _ "addend" e -> toExpr input e
    Rule _ "factor" e -> toExpr input e
    Rule _ "expr" e -> toExpr input e
    rule -> error $ "Not implemented expr: " ++ show rule

toParamRule input ast =
  case ast of
    Rule _ "params" (Seq _ [_, Rule _ _ p]) -> toParam input p
    rule -> error $ "not implemented paramrule: " ++ show rule

toParam input ast =
  case ast of
    Rule range "identifier" _  -> ParamId $ substr range input
    Rule range "decimal" _      -> ParamNum . read . B8.unpack $ substr range input
    rule -> error ("wrong param rule: " ++ show rule)

toFunRule input ast =
  case ast of
    Rule _ "fun" f -> toFun input f
    rule -> error ("wrong fun rule: " ++ show rule)

toFun input ast =
  case ast of
    Seq _ [Rule range "identifier" _, (Seq _ params), _, _, _, Rule _ _ expr] ->
      Fun (substr range input) (fmap (\p -> toParamRule input p) params) (toExpr input expr)
    rule -> error ("wrong fun rule: " ++ show rule)

listFrom ast =
  case ast of
    Seq _ rules -> rules
    Void -> []
    rule -> error $ "wrong listFrom: " ++ show rule

toFuns input ast =
  case ast of
    Rule _ "funs" (Seq _ [Rule _ "fun" fun, brfuns]) ->
      (toFun input fun):fmap (\(Rule _ _ (Seq _ [_, f])) -> toFunRule input f) (listFrom brfuns)
    rule -> error $ "wrong funs rule: " ++ (show rule)

toRuncall input ast =
  case ast of
    Rule _ "runcall" (Seq _ [_,_,Rule _ _ expr]) -> RunCall $ toExpr input expr
    rule -> error $ "wrong runcall rule: " ++ (show rule)

toProgram :: B8.ByteString -> AST -> Program
toProgram input ast =
  case ast of
    Rule _ "program" (Seq _ [funs, _, runcall, _]) -> Program (toFuns input funs) (toRuncall input runcall)
    rule -> error $ "wrong program rule: " ++ (show rule)

program input = do
  putStrLn $ B8.unpack input
  let result = parse grammar "program" input
  -- print $ result
  let ast = fromJust $ astFrom result
  pPrint $ toProgram input ast

main = do
  let input1 =
        B8.unlines
            [ "fat n = fat' n 1"
            , "fat' 1 acc = acc"
            , "fat' n acc = fat' (n - 1) (n * acc)"
            , "run (fat 10)"
            ]

  let input2 =
        B8.unlines
            [ "f x = x"
            , "g a b = a + b"
            , "fat n = fat' n 1"
            , "fat' 1 acc = acc"
            , "fat' n acc = fat' (n - 1) (n * acc)"
            , "expr a b = g 1 b - a * f 8"
            , "run (fat 5 + expr 4 9)"
            ]

  {-
  -- TODO: grammar should accept this one
  let input3 = "run (a + f (b * (c - d) + 5 - (8 - 7)))"
  -}
  program input1
  program input2
