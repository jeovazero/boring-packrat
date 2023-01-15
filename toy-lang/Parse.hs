{-# LANGUAGE OverloadedStrings #-}
module Parse where

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
import qualified Data.ByteString.Char8 as B8
import Data.List as L
import AST

unRule a (Rule a' _ r)
 | a == a' = r
 | otherwise = error . show $ B8.concat ["unRule ", a, " and ", a']

unSeq f (Seq _ a) = f a
unSeq f Void = f []

ruleToB8 input (Rule _ r _) = substr r input

transformAST s ast =
  case ast of
    Rule "program" _ (Seq _ decls) ->
      Program $ fmap (fromDeclslf s) decls

fromDeclslf s decls =
  let decs = unRule "decls" . unSeq (\[_,d,_] -> d) . unRule "decls_lf" $ decls
   in case decs of
        Rule "decls_expr" _ r -> toDexpr s r
        Rule "decls_dt" _ r -> toDdata s r
        f -> error (show f)

toDdata s r =
  case r of
    Seq _ [_,_,datatp,_,_,_,dataspec,dataspecs] ->
     let
       dspecs = (toDataSpec s dataspec):(toDataSpecs s dataspecs)
       (Seq _ [identifier,params]) = unRule "datatype" datatp
     in Ddata (UpperId $ s identifier) (toParams s params) dspecs
    Seq _ a -> error $ L.intercalate  "\n\n" (fmap show a) 
    a -> error $ show a

toDataSpec s r =
  case (unRule "datatype_spec" r) of
    Seq _ [did,dparams] -> DataSpec (UpperId $ s did) (toDParams s dparams)

toDataSpecs s rule =
  let
    toDSpec r =
      case r of
        Seq _ [_,_,_,param] -> toDataSpec s param
    r = unSeq (fmap toDSpec) $ unRule "datatype_specs" rule
  in
    r

toDParams s rule =
  let
    toDParam r =
      case r of
        Seq _ [_, param] ->
          case unRule "d_param" param of
            Rule "identifier" _ a -> LId . LowerId $ s a
            Rule "d_identifier" _ a -> UId . UpperId $ s a
            a -> error $ show a
    r = unSeq (fmap toDParam) $ unRule "d_params" rule
  in
    r

toDexpr s dec =
  case dec of
    Seq _ [identifier, params, _, equal, _, expr] ->
      Dexpr (LowerId $ s identifier) (toParams s params) (toExpr s expr)
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
    Rule "alt_expr" _ r -> toAltExpr s r
    Rule "aritm_expr" _ r -> toAritmExpr s r

toAltExpr s rule =
  case rule of
    Rule "parens" _ (Seq _ [_,_,p,_,_]) -> toExpr s p
    Rule "case_expr" _ p -> toCaseExpr s p
    Rule "guard_expr" _ p -> toGuardExpr s p
    Rule "let_expr" _ (Seq _ [_,_,decl,decls,_,_,_,expr]) ->
      let d = toDexpr s $ unRule "decls_expr" decl
          ds = fromDeclsExprs s decls
          decs = d:ds
        in
      LetExpr decs (toExpr s expr)
    Rule "decimal" _ d -> Decimal $ s d
    Rule "data_expr" _ p -> toDataExpr s p 
    Rule "call" _ p -> toCall s p
    Rule "identifier" _ p -> Identifier . LowerId $ s p
    Rule "lambda" _ p -> toLambda s p
    e -> error $ "At -> " ++ show (s rule) ++ " \n " ++ show e

toLambda s rule =
  case rule of
    Seq _ [_,identifier,_,_,_,expr] -> Lambda (LowerId $ s identifier) (toExpr s expr)

toDataExpr s rule =
  case rule of
    Seq _ [dataid,exprs] -> EData (UpperId $ s dataid) (toExprs s exprs)
  
toExprs s rule =
  let
    toExp r =
      case r of
        Seq _ [_, expr] -> toExpr s expr
    result = unSeq (fmap toExp) $ unRule "exprs" rule
  in
    result


toGuardExpr s rule =
  case rule of
    Seq _ [_,guardSts] -> GuardExpr (toGuardSts s guardSts)

toGuardSts s rule =
  case (unRule "guard_sts" rule) of
    Seq _ sts ->
      fmap (\(Seq _ [_,_,_,exprCond,_,_,_,exprOut]) -> (toExpr s exprCond, toExpr s exprOut)) sts
    Void  -> []


toCaseExpr s rule =
  case rule of
    Seq _ [_,_,expr,casests] -> CaseExpr (toExpr s expr) (toCaseSts s casests)

toCaseSts s rule =
  case (unRule "case_sts" rule) of
    Seq _ sts ->
      fmap (\(Seq _ [_,_,_,patWs,_,_,_,expr]) -> (toPattern s patWs, toExpr s expr))sts
    Void  -> []

toPattern s rule =
  case (unRule "pattern" rule) of
    Rule "hole" _ _ -> PHole
    Rule "decimal" _ d -> PDecimal (s d)
    Rule "d_pattern" _ (Seq _ [dId,patterns]) -> PData (UpperId $ s dId) (toPatterns s patterns)

toPatterns s rule =
  let
    toPat r =
      case r of
        Seq _ [_, pat] -> toPattern s pat
    result = unSeq (fmap toPat) $ unRule "patterns" rule
  in
    result

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
    Seq _ [cid, Rule "args" _ (Seq _ args)] ->
      Call (LowerId $ s cid) (fmap (Arg . toAltExpr s . unRule "arg" . unSeq (\[_, a] -> a)) args)
    Seq _ [cid, Rule "args" _ Void] ->
      Call (LowerId $ s cid) []
    a -> error $ show a

makeSubtr input r =
  case r of
    Rule _ a _ -> substr a input
    Seq a _ -> substr a input


