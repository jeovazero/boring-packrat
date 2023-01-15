module AST where

import qualified Data.ByteString.Char8 as B8

type BString = B8.ByteString

newtype Program = Program [Decls] deriving (Show, Eq)

data Decls
  = Dexpr LowerId [Param] Expr
  | Ddata UpperId [Param] [DataSpec]
  deriving (Show, Eq)

data DataSpec = DataSpec UpperId [Id] deriving (Show, Eq)

data Id = LId LowerId | UId UpperId deriving (Show, Eq)
newtype LowerId = LowerId BString deriving (Show,Eq) 
newtype UpperId = UpperId BString deriving (Show,Eq) 

newtype Param = Param BString deriving (Show,Eq)
data Expr
  = CaseExpr Expr [(Pattern, Expr)]
  | GuardExpr [(Expr,Expr)]
  | LetExpr [Decls] Expr
  | Decimal BString
  | EData UpperId [Expr]
  | Call LowerId [Arg] 
  | Identifier LowerId
  | Lambda LowerId Expr
  | AritmExpr Expr BinOp Expr
  deriving (Show,Eq)

data Pattern = PData UpperId [Pattern] | PDecimal BString | PId LowerId | PHole
  deriving (Show, Eq)

newtype BinOp = BinOp BString deriving (Show,Eq)
newtype Arg = Arg Expr deriving (Show,Eq)


