{
{-# LANGUAGE LambdaCase #-}
module Thale.Compiler.Parser (parse) where

import Thale.Compiler.Token (Token(..), LocatedToken(..))
import Thale.Compiler.Ast (Type(..), Expr(..))
import Thale.Compiler.Pretty (markRecursive)
import Thale.Compiler.Error (printParseError)
}

%name parse
%tokentype { LocatedToken }
%error { printParseError }

%token
  floatType { LocatedToken _ _ TokenFloatType }
  charType { LocatedToken _ _ TokenCharType }
  unitType { LocatedToken _ _ TokenUnitType }
  listType { LocatedToken _ _ TokenListType }
  boolType { LocatedToken _ _ TokenBoolType }
  ident { LocatedToken _ _ (TokenIdentifier $$) }
  float { LocatedToken _ _ (TokenFloat $$) }
  char { LocatedToken _ _ (TokenChar $$) }
  string { LocatedToken _ _ (TokenString $$) }
  bool { LocatedToken _ _ (TokenBool $$) }
  lparen { LocatedToken _ _ TokenLParen }
  rparen { LocatedToken _ _ TokenRParen }
  lbrace { LocatedToken _ _ TokenLBrace }
  rbrace { LocatedToken _ _ TokenRBrace }
  lbracket { LocatedToken _ _ TokenLBracket }
  rbracket { LocatedToken _ _ TokenRBracket }
  val { LocatedToken _ _ TokenVal }
  use { LocatedToken _ _ TokenUse }
  match { LocatedToken _ _ TokenMatch }
  with { LocatedToken _ _ TokenWith }
  colon { LocatedToken _ _ TokenColon }
  comma { LocatedToken _ _ TokenComma }
  semicolon { LocatedToken _ _ TokenSemicolon }
  equal { LocatedToken _ _ TokenEqual }
  underscore { LocatedToken _ _ TokenUnderscore }
  dot { LocatedToken _ _ TokenDot }
  pipe { LocatedToken _ _ TokenPipe }
  arrow { LocatedToken _ _ TokenArrow }
  plus { LocatedToken _ _ TokenPlus }
  minus { LocatedToken _ _ TokenMinus }
  star { LocatedToken _ _ TokenStar }
  slash { LocatedToken _ _ TokenSlash }
  percent { LocatedToken _ _ TokenPercent }
  carot { LocatedToken _ _ TokenCarot }
  less { LocatedToken _ _ TokenLess }
  greater { LocatedToken _ _ TokenGreater }
  notequal { LocatedToken _ _ TokenNotEqual }
  bang { LocatedToken _ _ TokenBang }
  logicalor { LocatedToken _ _ TokenLogicalOr }
  logicaland { LocatedToken _ _ TokenLogicalAnd }

%right equal
%left logicalor
%left logicaland
%nonassoc less greater notequal
%left plus minus
%left star slash percent
%right carot
%right bang

%%

Program :: { Expr }
Program : TopLevelExprList { markRecursive (Seq $1) }

TopLevelExprList :: { [Expr] }
TopLevelExprList : TopLevelExpr TopLevelExprList { $1 : $2 }
    | TopLevelExpr { [$1] }

TopLevelExpr :: { Expr }
TopLevelExpr : use ModulePath { UseExpr $2 } 
    | val ident lparen Params rparen arrow TypeTok lbrace ExprList rbrace { FunDecl $2 $4 $7 (Seq $9) False }
    | val ident lparen Params rparen lbrace ExprList rbrace { FunDecl $2 $4 TypeInfer (Seq $7) False }
    | Expr { $1 }

ModulePath :: { String }
ModulePath : ident dot ModulePath { $1 ++ "." ++ $3 }
           | ident { $1 }

ExprList :: { [Expr] }
ExprList : Expr ExprList { $1 : $2 }
         | Expr { [$1] }

Expr :: { Expr }
Expr : FunDeclExpr { $1 } 
     | MatchExpr { $1 }

FunDeclExpr :: { Expr }
FunDeclExpr
    : val ident lparen Params rparen arrow TypeTok lbrace ExprList rbrace { FunDecl $2 $4 $7 (Seq $9) False }
    | val ident lparen Params rparen lbrace ExprList rbrace { FunDecl $2 $4 TypeInfer (Seq $7) False }

MatchExpr :: { Expr }
MatchExpr
    : match Expr with MatchArms { Match $2 $4 }
    | AssignmentExpr { $1 }

BlockExpr :: { Expr }
BlockExpr
    : lbrace ExprList rbrace { Seq $2 }

MatchArms :: { [(Expr, Expr)] }
MatchArms
    : pipe Pattern arrow Expr MatchArms { ($2, $4) : $5 }
    | pipe Pattern arrow Expr { [($2, $4)] }

Pattern :: { Expr }
Pattern
    : float { FloatLit $1 }
    | char { CharLit $1 }
    | bool { BoolLit $1 }
    | string { ListLit (map CharLit $1) }
    | ident { Var $1 }
    | underscore { Wildcard }
    | lbrace ExprList rbrace { Seq $2 }

AssignmentExpr :: { Expr }
AssignmentExpr : ident equal AssignmentExpr { Assign $1 $3 }
               | LogicalOrExpr { $1 }
               | BlockExpr { $1 }

LogicalOrExpr :: { Expr }
LogicalOrExpr : LogicalAndExpr { $1 }
    | LogicalOrExpr logicalor LogicalAndExpr { LogicalOr $1 $3 }

LogicalAndExpr :: { Expr }
LogicalAndExpr : EqualityExpr { $1 }
    | LogicalAndExpr logicaland EqualityExpr { LogicalAnd $1 $3 }

EqualityExpr :: { Expr }
EqualityExpr : RelationalExpr { $1 }
    | EqualityExpr notequal RelationalExpr { NotEqual $1 $3 }

RelationalExpr :: { Expr }
RelationalExpr : AdditiveExpr { $1 }
    | RelationalExpr less AdditiveExpr { Less $1 $3 }
    | RelationalExpr greater AdditiveExpr { Greater $1 $3 }

AdditiveExpr :: { Expr }
AdditiveExpr : MultiplicativeExpr { $1 }
    | AdditiveExpr plus MultiplicativeExpr { Add $1 $3 }
    | AdditiveExpr minus MultiplicativeExpr { Sub $1 $3 }

MultiplicativeExpr :: { Expr }
MultiplicativeExpr : PowerExpr { $1 }
    | MultiplicativeExpr star PowerExpr { Mul $1 $3 }
    | MultiplicativeExpr slash PowerExpr { Div $1 $3 }
    | MultiplicativeExpr percent PowerExpr { Mod $1 $3 }

PowerExpr :: { Expr }
PowerExpr : UnaryExpr { $1 }
    | PowerExpr carot UnaryExpr { Pow $1 $3 }

UnaryExpr :: { Expr }
UnaryExpr : bang UnaryExpr { Not $2 }
    | minus UnaryExpr { Sub (FloatLit 0) $2 }
    | plus UnaryExpr { $2 }
    | CallExpr { $1 }

PrimaryExpr :: { Expr }
PrimaryExpr : ident { Var $1 }
    | float { FloatLit $1 }
    | char { CharLit $1 }
    | bool { BoolLit $1 }
    | string { ListLit (map CharLit $1) }
    | lparen Expr rparen { $2 }
    | lbracket Elements rbracket { ListLit $2 }
    | val ident colon TypeTok equal Expr { ValDecl $2 $4 $6 }
    | val ident equal Expr { ValDecl $2 TypeInfer $4 }
    | PrimaryExpr dot ident { PropertyAccess $1 $3 }
    | val lparen IdList rparen colon TypeList equal Expr { ValDeclTuple $3 $6 $8 }
    | lparen ExprListComma rparen { Tuple $2 }

IdList :: { [String] }
IdList : ident comma IdList { $1 : $3 }
       | ident { [$1] }

ExprListComma :: { [Expr] }
ExprListComma
    : Expr comma ExprListComma { $1 : $3 }
    | Expr comma Expr { [$1, $3] }
    | Expr { [$1] }

CallExpr :: { Expr }
CallExpr : PrimaryExpr { $1 }
         | CallExpr lparen Args rparen { Call $1 $3 }

Args :: { [Expr] }
Args : Expr comma Args { $1 : $3 }
     | Expr { [$1] }
     | { [] }

Params :: { [(String, Type)] }
Params : ident colon TypeTok comma Params { ($1, $3) : $5 }
    | ident colon TypeTok { [($1, $3)] }
    | ident comma Params { ($1, TypeInfer) : $3 }
    | ident { [($1, TypeInfer)] }
    | { [] }

Elements :: { [Expr] }
Elements : Expr comma Elements { $1 : $3 }
         | Expr { [$1] }
         | { [] } 

TypeTok :: { Type }
TypeTok : floatType { TypeFloat }
        | charType { TypeChar }
        | unitType { TypeUnit }
        | boolType { TypeBool }
        | listType lbracket TypeTok rbracket { TypeList $3 } 
        | lparen TypeList rparen { TypeTuple $2 }

TypeList :: { [Type] }
TypeList : TypeTok comma TypeList { $1 : $3 }
         | TypeTok { [$1] }
