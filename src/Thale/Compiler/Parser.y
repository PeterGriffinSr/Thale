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
  intType { LocatedToken _ _ TokenIntType }
  floatType { LocatedToken _ _ TokenFloatType }
  charType { LocatedToken _ _ TokenCharType }
  unitType { LocatedToken _ _ TokenUnitType }
  listType { LocatedToken _ _ TokenListType }
  boolType { LocatedToken _ _ TokenBoolType }
  
  ident { LocatedToken _ _ (TokenIdentifier $$) }
  intlit { LocatedToken __ (TokenInt $$) }
  floatlit { LocatedToken _ _ (TokenFloat $$) }
  charlit { LocatedToken _ _ (TokenChar $$) }
  stringlit { LocatedToken _ _ (TokenString $$) }
  boollit { LocatedToken _ _ (TokenBool $$) }

  lparen { LocatedToken _ _ TokenLParen }
  rparen { LocatedToken _ _ TokenRParen }
  lbrace { LocatedToken _ _ TokenLBrace }
  rbrace { LocatedToken _ _ TokenRBrace }
  lbracket { LocatedToken _ _ TokenLBracket }
  rbracket { LocatedToken _ _ TokenRBracket }
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
  consop { LocatedToken _ _ TokenConsOp }

  letkw { LocatedToken _ _ TokenLet }
  reckw { LocatedToken _ _ TokenRec }
  matchkw { LocatedToken _ _ TokenMatch }
  withkw { LocatedToken _ _ TokenWith }
  effectkw { LocatedToken _ _ TokenEffect }
  dokw { LocatedToken _ _ TokenDo }
  ifkw { LocatedToken _ _ TokenIf }
  thenkw { LocatedToken _ _ TokenThen }
  elsekw { LocatedToken _ _ TokenElse }

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
Program : TopLevelList { markRecursive (Seq $1) }

TopLevelList :: { [Expr] }
TopLevelList : TopLevel TopLevelList { $1 : $2 }
  | TopLevel { [$1] }

TopLevel :: { Expr }
TopLevel : EffectDecl { $1 }
         | TypeDecl { $1 }
         | FunDeclGroup { $1 }
         | Expr { $1 }

EffectDecl :: { Expr }
EffectDecl : effectkw ident lbrace EffectMembers rbrace { EffectDecl $2 $4 }

EffectMembers :: { [(String, Type)] }
EffectMembers : EffectMember comma EffectMembers { $1 : $3 }
              | EffectMember { [$1] }
              | { [] }

EffectMember :: { (String, Type) }
EffectMember
  : ident colon TypeSig { ($1, $3) }

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
