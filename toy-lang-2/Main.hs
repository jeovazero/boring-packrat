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
import Debug.Trace
--
type BString = B8.ByteString

n = NonTerminal
_ParenLeft  = Terminal $ Lit W._parenleft  -- '('
_ParenRight = Terminal $ Lit W._parenright -- ')'
_Times      = Terminal $ Lit W._asterisk   -- '*'
_Plus       = Terminal $ Lit W._plus       -- '+'
_Minus      = Terminal $ Lit W._hyphen     -- '-'
_Apostrofe  = Terminal $ Lit W._quotesingle -- "'" 
_Underscore = Terminal $ Lit W._underscore -- "'" 
_Equal      = Terminal $ Lit W._equal
_Comma      = Terminal $ Lit W._comma
_Lower      = Terminal AlphaLower
_Upper      = Terminal AlphaUpper
_Lf         = Choice [_WSP, _CRLF, _CR, _LF]

spacesGrammar :: Grammar
spacesGrammar =
  [ ("_", Many0 _WSP)
  , ("__", Many1 _WSP)
  , ("lf", Choice [_CR, _LF, _CRLF])
  , ("_lf", Many0 _Lf)
  , ("__lf", Many1 _Lf)
  ]

symbolsGrammar :: Grammar
symbolsGrammar =
  [ ("equal", _Equal)
  , ("apostrophe", _Apostrofe)
  , ("plus", _Plus)
  , ("comma", _Comma)
  , ("minus", _Minus)
  , ("times", _Times)
  , ("decimal", Many1 _Digit)
  ]

specialSymbolsGrammar :: Grammar
specialSymbolsGrammar =
  [ ("hole", _Underscore)
  , ("r_arrow", litBS "->")
  , ("sep", litBS "|")
  ]

operatorsGrammar :: Grammar
operatorsGrammar =
  [ ("bin_op", Choice [n"plus", n"minus", n"eq", n"neq", n"times"])
  , ("eq", litBS "==")
  , ("neq", litBS "/=")
  ]

wordsGrammar :: Grammar
wordsGrammar =
  [ ("identifier", Sequence [n"w", Many0 $ n"wW", Many0 _Apostrofe])
  , ("d_identifier", Sequence [n"W", Many0 $ n"wW", Many0 _Apostrofe])
  , ("wW", _Alpha)
  , ("W", _Upper)
  , ("w", _Lower)
  ]

keywordsGrammar :: Grammar
keywordsGrammar =
  [ ("main", litBS "main")
  , ("case", litBS "case")
  , ("guard", litBS "guard")
  , ("let", litBS "let")
  , ("in", litBS "in")
  , ("case", litBS "case")
  , ("data", litBS "data")
  ]
mainGrammar :: Grammar
mainGrammar =
  [ ("program", Many1 $ n"decls_lf")
  , ("decls_lf", Sequence [n"_lf", n"decls", n"_lf"])
  , ("decls", Choice [n"decls_expr", n"decls_dt"])
  , ("decls_expr", Sequence [n"identifier", n"params", n"_lf", n"equal", n"_lf", n"expr" ])
  , ("decls_dt", Sequence [n"data", n"_", n"datatype", n"_lf", n"datatype_inst"])
  , ("params", Many0 $ Sequence [n"__", n"identifier"])
  , ("datatype",Sequence [n"d_identifier", n"params"])
  , ("datatype_inst",Sequence [n"d_identifier", n"d_params"])
  , ("d_params", Choice [n"identifier", n"d_identifier"])
  , ("expr", Choice [n"aritm_expr", n"alt_expr"])
  , ("aritm_expr", Sequence [n"alt_expr", n"_lf", n"bin_op", n"_lf", n"expr"])
  , ("alt_expr", Choice [n"parens", n"case_expr", n"guard_expr", n"let_expr", n"decimal", n"data", n"call", n"lambda"])
  , ("lambda", Sequence [litBS "\\", n"identifier", n"_lf", n"r_arrow", n"_lf", n"expr"])
  , ("call", Sequence [n"identifier", n"args"])
  , ("args", Many0 $ Sequence [n"__", n"arg"])
  , ("arg", Choice [n"decimal", n"parens", n"identifier"])
  , ("parens", Sequence [_ParenLeft,n"_",n"expr",n"_", _ParenRight])
  , ("data", Sequence [n"d_identifier", n"exprs"])
  , ("exprs", Many0 $ Sequence [n"__", n"expr"])
  , ("let_expr", Sequence [n"let", n"__lf", n"decls_expr", n"decls_exprs",n"__lf", n"in", n"__lf", n"expr"])
  , ("decls_exprs", Many0 $ Sequence [n"sep_let", n"decls_expr"])
  , ("sep_let", Sequence [n"_lf", n"comma", n"_lf"])
  , ("guard_expr", Sequence [n"guard", n"guard_sts"])
  , ("guard_sts", Many1 $ Sequence [n"_lf", n"sep", n"__", n"expr", n"_lf", n"r_arrow", n"_lf", n"expr"])
  , ("case_expr", Sequence [n"case",n"__",n"expr",n"case_sts"])
  , ("case_sts", Many1 $ Sequence [n"_lf", n"sep", n"__", n"pattern_ws", n"_lf", n"r_arrow", n"_lf", n"expr"])
  , ("pattern_ws", Sequence [n"_",n"pattern", n"_"])
  , ("pattern", Choice [n"hole", n"decimal", n"d_pattern"])
  , ("d_pattern", Sequence [n"d_identifier", n"patterns"])
  , ("patterns", Many0 $ Sequence [n"__", n"pattern"])
  ]

grammar =
  concat
    [ mainGrammar
    , wordsGrammar
    , specialSymbolsGrammar
    , operatorsGrammar
    , keywordsGrammar
    , spacesGrammar
    , symbolsGrammar
    ]

unRule a (Rule _ a' r)
 | a == a' = r
 | otherwise = error . show $ B8.concat ["unRule ", a, " and ", a']

unSeq f (Seq _ a) = f a
unSeq f Void = f []

ruleToB8 input (Rule r _ _) = substr r input

transformAST s ast =
  case ast of
    Rule _ "program" (Seq _ decls) ->
        Program $ fmap (fromDeclslf s) decls

fromDeclslf s decls =
  let decs = unRule "decls" . unSeq (\[_,d,_] -> d) . unRule "decls_lf" $ decls
   in case decs of
        Rule _ "decls_expr" r -> toDexpr s r
        Rule _ "decls_dt" r -> Ddata r
        f -> error (show f)

toDexpr s dec =
  case dec of
    Seq _ [identifier, params, _, equal, _, expr] ->
      Dexpr (Id $ s identifier) (toParams s params) (toExpr s expr)
    a -> error $ show a

toParams s rule =
  let
    toParam r =
      case r of
        Seq _ [_, param] -> Param $ s param
    r = unSeq (fmap toParam) $ unRule "params" rule
  in
    r
  
toExpr s rule =
  case (unRule "expr" rule) of
    Rule _ "alt_expr" r -> toAltExpr s r
    Rule _ "aritm_expr" r -> toAritmExpr s r

toAltExpr s rule =
  case rule of
    Rule _ "parens" (Seq _ [_,_,p,_,_]) -> toExpr s p
    Rule _ "case_expr" p -> toCase s p
    Rule _ "guard_expr" p -> GuardExpr $ s p
    Rule _ "let_expr" (Seq _ [_,_,decl,decls,_,_,_,expr]) ->
      let d = toDexpr s $ unRule "decls_expr" decl
          ds = fromDeclsExprs s decls
          decs = d:ds
        in
      LetExpr decs (toExpr s expr)
    Rule _ "decimal" d -> Decimal $ s d
    Rule _ "data" p -> error $ show p
    Rule _ "call" p -> toCall s p
    Rule _ "lambda" p -> error $ show p
    e -> error $ show (s rule) ++ " \n " ++ show e

toCase s rule =
  case rule of
    Seq _ [_,_,expr,casests] -> CaseExpr (toExpr s expr) (toCaseSts s casests)

toCaseSts s rule =
  case (unRule "case_sts" rule) of
    Seq _ [     ] ->
    Void  -> []

fromDeclsExprs s rule =
  case (unRule "decls_exprs" rule) of
    Void -> []
    Seq _ r -> fmap (\(Seq _ [_,dec]) -> toDexpr s $ unRule "decls_expr" dec) r

toAritmExpr s rule =
  case rule of
    Seq _ [altExpr, _, binOp, _, expr] ->
      AritmExpr (toAltExpr s (unRule "alt_expr" altExpr)) (BinOp $ s binOp) (toExpr s expr)

toCall s rule =
  case rule of
    Seq _ [id, Rule _ "args" (Seq _ args)] ->
      Call (Id $ s id) (fmap (Arg . toAltExpr s . unRule "arg" . unSeq (\[_, a] -> a)) args)
    Seq _ [id, Rule _ "args" Void] ->
      Call (Id $ s id) []
    a -> error $ show a

makeSubtr input r =
  case r of
    Rule a _ _ -> substr a input
    Seq a _ -> substr a input

program input = do
  putStrLn $ B8.unpack input
  let result = parse grammar "program" input
  -- print $ result
  let ast = fromJust $ astFrom result
  let s = makeSubtr input
  pPrint $ transformAST s ast

newtype Program = Program [Decls] deriving (Show, Eq)

data Decls
  = Dexpr Id [Param] Expr
  | Ddata AST
  deriving (Show, Eq)

newtype Id = Id BString deriving (Show,Eq) 
newtype Param = Param BString deriving (Show,Eq)
data Expr
  = CaseExpr Expr [(Pattern, Expr)]
  | GuardExpr Expr [(Expr,Expr)]
  | LetExpr [Decls] Expr
  | Decimal BString
  | EData BString [Expr]
  | Call Id [Arg] 
  | Lambda Id Expr
  | AritmExpr Expr BinOp Expr
  deriving (Show,Eq)

data Pattern = PData Id [Pattern] | PDecimal BString | PId Id | PHole
  deriving (Show, Eq)

newtype BinOp = BinOp BString deriving (Show,Eq)
newtype Arg = Arg Expr deriving (Show,Eq)

main = do
  let input1 =
        B8.unlines
            [ --"fat n = fat' n 1"
            --, "fat' a acc = acc"
            --, "fat' n acc = fat' (n - 1) (n * acc)"
            -- "r = let x = 1 + 2, y = 2 in x - y + x - y + x + x",
            "x = case z | True -> a b | False -> b + 2"
            ]

  {-
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

  -- TODO: grammar should accept this one
  let input3 = "run (a + f (b * (c - d) + 5 - (8 - 7)))"
  -}
  program input1
  -- program input2
