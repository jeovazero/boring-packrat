{-# LANGUAGE OverloadedStrings #-}
import BoringPackrat (
    parse,
    astFrom,
    AST(..),
    Terminal'(..),
    PEG(..),
    Grammar
  )
import BoringPackrat.Terminals
import BoringPackrat.PrettyPrint (prettyPrint)
import qualified Data.Word8 as W
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)

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
  , ("factor", Choice [n"parens", n"number", n"call"])
  , ("parens", Sequence [_ParenLeft,n"_",n"expr",n"_", _ParenRight])
  , ("fun", Sequence [n"identifier", Many0 $ n"params", n"_", _Equal, n"_", n"expr"])
  , ("params", Sequence [n"__", n"param"])
  , ("param", Choice [n"identifier", n"number"])
  , ("call", Sequence [n"identifier", Many0 $ n"args"])
  , ("args", Sequence [n"__", n"argument"])
  , ("argument", Choice [n"identifier", n"expr"])
  , ("identifier", Sequence [Many1 $ n"wW", Many0 _Apostrofe])
  , ("wW", _Alpha)
  , ("W", _Upper)
  , ("w", _Lower)
  , ("number", Many1 _Digit)
  , ("equal", _Equal)
  , ("apostrofe", _Apostrofe)
  , ("plusminus", Choice [_Plus, _Minus])
  , ("times", _Times)
  , ("run", litBS "run")
  , ("_", Many0 _WSP)
  , ("__", Many1 _WSP)
  , ("lf", Choice [_CR, _LF, _CRLF])
  , ("_lf", Many0 $ _Lf)
  , ("__lf", Many1 $ _Lf)
  ]

type Str = B8.ByteString

data Program = Program [Fun] RunCall deriving (Show)
data Fun = Fun Identifier [Params] Expr deriving (Show)
data Identifier = Identifier Str deriving (Show)
data Params = ParamId Identifier | ParamNum Number deriving (Show)
data Number = Number Int deriving (Show)
data RunCall = RunCall Expr deriving (Show)
data Expr
    = Add Expr Expr
    | Mult Expr Expr
    | Decimal Int
    | Call Identifier [Args] 
    deriving (Show)
data Args
    = ArgId Identifier
    | ArgExpr Expr
    deriving (Show)

transformAST :: AST -> Program
transformAST ast = undefined

main = do
  let input =
        B8.unlines
            [ "f x = x"
            , "g a b = a + b"
            , "fat n = fat' n 1"
            , "fat' 1 acc = acc"
            , "fat' n acc = fat (n - 1) (n * acc)"
            , "expr a b = g 1 b - a * f 8"
            , "run (fat 5 + expr 4 9)"
            ]

  putStrLn $ B8.unpack input
  let result = parse grammar "program" input
  print $ result
  -- let ast = fromJust $ astFrom result

  -- prettyPrint input ast
--  print $ transformAST ast

