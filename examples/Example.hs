{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
import BoringPackrat (
    parse,
    astFrom,
    AST(..),
    Terminal'(..),
    PEG(..),
    Grammar, substr
  )
import BoringPackrat.PrettyPrint (prettyPrint)
import BoringPackrat.Terminals
import qualified Data.Word8 as W
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)

n = NonTerminal
_ParenLeft  = Terminal $ Lit W._parenleft  -- '('
_ParenRight = Terminal $ Lit W._parenright -- ')'
_Times      = Terminal $ Lit W._asterisk   -- '*'
_Plus       = Terminal $ Lit W._plus       -- '+'

grammar :: Grammar
grammar =
  [ ("Expr", Choice [n"Add",n"Addend"])
  , ("Add", Sequence [n"Addend",_Plus,n"Expr"])
  , ("Addend", Choice [n"Mult",n"Factor"])
  , ("Mult", Sequence [n"Factor",_Times,n"Addend"])
  , ("Factor", Choice [n"Parens",n"Decimal"])
  , ("Parens", Sequence [_ParenLeft,n"Expr",_ParenRight])
  , ("Decimal", Many1 $ Terminal Digit)
  ]

data Expr
    = Add Expr Expr
    | Mult Expr Expr
    | Decimal Int
    deriving (Show)

transformAST :: B8.ByteString -> AST -> Expr
transformAST input ast =
  case ast of
    Rule _ "Add" (Seq _ [a,_,b]) ->
      Add (transformAST input a) (transformAST input b)
    Rule _ "Mult" (Seq _ [a,_,b]) ->
      Mult (transformAST input a) (transformAST input b)
    Rule _ "Parens" (Seq _ [_,e,_]) ->
      transformAST input e
    Rule _ "Decimal" (Seq r _) ->
      Decimal (read . B8.unpack $ substr r input)
    Rule _ _ e -> transformAST input e
    r -> error $ "Not implemented " ++ (show r)

main = do
  let input = "(1+2)*3"
  let result = parse grammar "Expr" input
  let ast = fromJust $ astFrom result

  prettyPrint input ast
  print $ transformAST input ast
