module Thale.Compiler.Ast (Type (..), Expr (..)) where

import Prelude

data Type
  = TypeFloat
  | TypeChar
  | TypeUnit
  | TypeList Type
  | TypeBool
  | TypeTuple [Type]
  | TypeInfer
  deriving (Eq, Show)

data Expr
  = FloatLit Double
  | CharLit Char
  | ListLit [Expr]
  | BoolLit Bool
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Less Expr Expr
  | Greater Expr Expr
  | Mod Expr Expr
  | Pow Expr Expr
  | Not Expr
  | NotEqual Expr Expr
  | LogicalOr Expr Expr
  | LogicalAnd Expr Expr
  | Assign String Expr
  | ValDecl String Type Expr
  | ValDeclTuple [String] Type Expr
  | FunDecl String [(String, Type)] Type Expr Bool
  | PropertyAccess Expr String
  | Call Expr [Expr]
  | Match Expr [(Expr, Expr)]
  | Wildcard
  | Seq [Expr]
  | UseExpr String
  | Tuple [Expr]
  deriving (Eq, Show)