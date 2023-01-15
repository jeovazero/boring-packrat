{-# LANGUAGE OverloadedStrings #-}
module Grammar where

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
import qualified BoringPackrat.Chars as C


n = NonTerminal
_ParenLeft  = Terminal $ Lit C._parenleft  -- '('
_ParenRight = Terminal $ Lit C._parenright -- ')'
_Times      = Terminal $ Lit C._asterisk   -- '*'
_Plus       = Terminal $ Lit C._plus       -- '+'
_Minus      = Terminal $ Lit C._hyphen     -- '-'
_Apostrofe  = Terminal $ Lit C._quotesingle -- "'" 
_Underscore = Terminal $ Lit C._underscore -- "'" 
_Equal      = Terminal $ Lit C._equal
_Comma      = Terminal $ Lit C._comma
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
  , ("decls_dt", Sequence [n"data", n"_", n"datatype", n"_lf", n"equal", n"_lf", n"datatype_spec", n"datatype_specs"])
  , ("params", Many0 $ Sequence [n"__", n"identifier"])
  , ("datatype",Sequence [n"d_identifier", n"params"])
  , ("datatype_specs", Many0 $ Sequence [n"_lf", n"sep", n"_", n"datatype_spec"])
  , ("datatype_spec",Sequence [n"d_identifier", n"d_params"])
  , ("d_param", Choice [n"identifier", n"d_identifier"])
  , ("d_params", Many0 $ Sequence [n"__", n"d_param"])
  , ("expr", Choice [n"aritm_expr", n"alt_expr"])
  , ("aritm_expr", Sequence [n"alt_expr", n"_lf", n"bin_op", n"_lf", n"expr"])
  , ("alt_expr", Choice [n"parens", n"case_expr", n"guard_expr", n"let_expr", n"decimal", n"data_expr", n"call", n"lambda", n"identifier"])
  , ("lambda", Sequence [litBS "\\", n"identifier", n"_lf", n"r_arrow", n"_lf", n"expr"])
  , ("call", Sequence [n"identifier", n"args"])
  , ("args", Many1 $ Sequence [n"__", n"arg"])
  , ("arg", Choice [n"decimal", n"parens", n"identifier"])
  , ("parens", Sequence [_ParenLeft,n"_",n"expr",n"_", _ParenRight])
  , ("data_expr", Sequence [n"d_identifier", n"exprs"])
  , ("exprs", Many0 $ Sequence [n"__", n"expr"])
  , ("let_expr", Sequence [n"let", n"__lf", n"decls_expr", n"decls_exprs",n"__lf", n"in", n"__lf", n"expr"])
  , ("decls_exprs", Many0 $ Sequence [n"sep_let", n"decls_expr"])
  , ("sep_let", Sequence [n"_lf", n"comma", n"_lf"])
  , ("guard_expr", Sequence [n"guard", n"guard_sts"])
  , ("guard_sts", Many1 $ Sequence [n"_lf", n"sep", n"__", n"expr", n"_lf", n"r_arrow", n"_lf", n"expr"])
  , ("case_expr", Sequence [n"case",n"__",n"expr",n"case_sts"])
  , ("case_sts", Many1 $ Sequence [n"_lf", n"sep", n"__", n"pattern", n"_lf", n"r_arrow", n"_lf", n"expr"])
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


