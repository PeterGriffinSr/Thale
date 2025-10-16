{-# LANGUAGE LambdaCase #-}

module Thale.Compiler.Token (Token (..), LocatedToken (..), tokenToString) where

import Prelude (Bool, Char, Double, Eq, Int, Show, String, show)

data LocatedToken = LocatedToken
  { locLine :: !Int,
    locCol :: !Int,
    locTok :: Token
  }
  deriving (Show, Eq)

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
  | TokenEof
  deriving (Show, Eq)

tokenToString :: Token -> String
tokenToString = \case
  TokenPlus -> "+"
  TokenMinus -> "-"
  TokenStar -> "*"
  TokenSlash -> "/"
  TokenPercent -> "%"
  TokenEqual -> "="
  TokenArrow -> "->"
  TokenPipe -> "|"
  TokenNotEqual -> "<>"
  TokenLogicalAnd -> "&&"
  TokenLogicalOr -> "||"
  TokenRBrace -> "}"
  TokenRParen -> ")"
  TokenRBracket -> "]"
  TokenLBrace -> "{"
  TokenLParen -> "("
  TokenLBracket -> "["
  TokenEof -> "<EOF>"
  _ -> show ""