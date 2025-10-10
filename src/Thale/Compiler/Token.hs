module Thale.Compiler.Token (Token (..)) where

import Prelude

data Token
  = TokenFloat Double
  | TokenChar Char
  | TokenString String
  | TokenIdentifier String
  | TokenBool Bool
  | TokenVal
  | TokenIn
  | TokenType
  | TokenUse
  | TokenMatch
  | TokenWith
  | TokenFun
  | TokenFloatType
  | TokenCharType
  | TokenUnitType
  | TokenListType
  | TokenBoolType
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenLBrace
  | TokenRBrace
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenSlash
  | TokenPercent
  | TokenCarot
  | TokenLess
  | TokenGreater
  | TokenBang
  | TokenColon
  | TokenComma
  | TokenPipe
  | TokenDot
  | TokenEqual
  | TokenUnderscore
  | TokenSemicolon
  | TokenNotEqual
  | TokenLogicalOr
  | TokenLogicalAnd
  | TokenArrow
  deriving (Show, Eq)