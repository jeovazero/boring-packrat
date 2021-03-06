## Boring Packrat

A packrat parser using [Boring Haskell](https://www.simplehaskell.org/) (a.k.a Simple Haskell).

The initial objective of the project was to make an email validator to use in another project,
but it would be very easy just use a library from Hackage, so why not make a parser for fun?

### Examples

#### Email

The email grammar was based on [RFC 5321](https://www.rfc-editor.org/rfc/pdfrfc/rfc5321.txt.pdf).

[Check out the grammar here](https://github.com/jeovazero/boring-packrat/blob/main/lib/BoringPackrat/Email.hs#L88)

#### Toy-Grammar

```bnf
Expr    <- Add | Addend
Add     <- Addend "+" Expr
Addend  <- Mult | Factor
Mult    <- Factor "*" Addend
Factor  <- Parens | Decimal
Parens  <- "(" Expr ")"
Decimal <- [0-9]+
```

```hs
{-# LANGUAGE OverloadedStrings #-}
import BoringPackrat (
    parse,
    astFrom,
    AST(..),
    Terminal'(..),
    PEG(..),
    Grammar
  )
import BoringPackrat.PrettyPrint (prettyPrint)
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

transformAST :: AST -> Expr
transformAST ast =
  case ast of
    Rule _ "Add" (Seq _ [a,_,b]) ->
      Add (transformAST a) (transformAST b)
    Rule _ "Mult" (Seq _ [a,_,b]) ->
      Mult (transformAST a) (transformAST b)
    Rule _ "Parens" (Seq _ [_,e,_]) ->
      transformAST e
    Rule _ "Decimal" (Str d) ->
      Decimal (read $ B8.unpack d)
    Rule _ _ e -> transformAST e
    _ -> error "Not implemented"

main = do
  let input = "(1+2)*3"
  let result = parse grammar "Expr" input
  let ast = fromJust $ astFrom result

  prettyPrint input ast
  print $ transformAST ast
```

Output:

```
Rule Expr -> (0,6) -> "(1+2)*3"
  Rule Addend -> (0,6) -> "(1+2)*3"
    Rule Mult -> (0,6) -> "(1+2)*3"
      Sequence -> (0,6) -> "(1+2)*3"
        -Rule Factor -> (0,4) -> "(1+2)"
          Rule Parens -> (0,4) -> "(1+2)"
            Sequence -> (0,4) -> "(1+2)"
              -Str "("
              -Rule Expr -> (1,3) -> "1+2"
                Rule Add -> (1,3) -> "1+2"
                  Sequence -> (1,3) -> "1+2"
                    -Rule Addend -> (1,1) -> "1"
                      Rule Factor -> (1,1) -> "1"
                        Rule Decimal -> (1,1) -> "1"
                          Str "1"
                    -Str "+"
                    -Rule Expr -> (3,3) -> "2"
                      Rule Addend -> (3,3) -> "2"
                        Rule Factor -> (3,3) -> "2"
                          Rule Decimal -> (3,3) -> "2"
                            Str "2"
              -Str ")"
        -Str "*"
        -Rule Addend -> (6,6) -> "3"
          Rule Factor -> (6,6) -> "3"
            Rule Decimal -> (6,6) -> "3"
              Str "3"
Mult (Add (Decimal 1) (Decimal 2)) (Decimal 3)
```

> a boring haskell project by jeovazero :pensive:
