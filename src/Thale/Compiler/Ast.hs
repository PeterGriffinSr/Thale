module Thale.Compiler.Ast (Type (..), Expr (..)) where

import Prelude (Bool, Char, Double, Eq, Int, Show, String)

data Type
  = TypeInt
  | TypeFloat
  | TypeChar
  | TypeString
  | TypeUnit
  | TypeList Type
  | TypeBool
  | TypeTuple [Type]
  | TypeInfer
  deriving (Eq, Show)

data Expr
  = IntLit Int
  | FloatLit Double
  | CharLit Char
  | StringLit String
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
  | LetDecl String Type Expr
  | LetDeclTuple [String] [Type] Expr
  | FunDecl String [(String, Type)] Type Expr Bool
  | PropertyAccess Expr String
  | Call Expr [Expr]
  | Match Expr [(Expr, Expr)]
  | Wildcard
  | Seq [Expr]
  | Tuple [Expr]
  deriving (Eq, Show)