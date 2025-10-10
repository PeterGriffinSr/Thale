{
{-# LANGUAGE Unsafe #-}
module Thale.Compiler.Lexer (alexScanTokens) where
import Thale.Compiler.Token (Token(..))

import Prelude
}

%wrapper "basic"

tokens :- 

    $white+;
    "#".*;
    "#*" (.| \n )* "*#" ;


    [0-9]+ "." [0-9]+ { \s -> TokenFloat (read s) }
    [0-9]+ { \s -> TokenFloat (read s) }
    \"([^\\\"]|\\.)*\" { \s -> TokenString (read s) }
    \'([^\\\']|\\.)\' { \s -> TokenChar (read s) }

    [a-zA-Z][a-zA-Z0-9\'_]* { \s -> case s of 
                                    "val" -> TokenVal
                                    "type" -> TokenType
                                    "use" -> TokenUse
                                    "match" -> TokenMatch
                                    "with" -> TokenWith
                                    "Float" -> TokenFloatType
                                    "Char" -> TokenCharType
                                    "Unit" -> TokenUnitType
                                    "Bool" -> TokenBoolType
                                    "List" -> TokenListType
                                    "True" -> TokenBool True
                                    "False" -> TokenBool False
                                    _ -> TokenIdentifier s }

    "(" { const TokenLParen }
    ")" { const TokenRParen }
    "[" { const TokenLBracket }
    "]" { const TokenRBracket }
    "{" { const TokenLBrace }
    "}" { const TokenRBrace }
    "+" { const TokenPlus }
    "-" { const TokenMinus }
    "*" { const TokenStar }
    "/" { const TokenSlash }
    "%" { const TokenPercent }
    "^" { const TokenCarot }
    "<" { const TokenLess }
    ">" { const TokenGreater }
    "!" { const TokenBang }
    ":" { const TokenColon }
    "," { const TokenComma }
    "|" { const TokenPipe }
    "." { const TokenDot }
    "=" { const TokenEqual }
    "_" { const TokenUnderscore }
    ";" { const TokenSemicolon }
    "<>" { const TokenNotEqual }
    "||" { const TokenLogicalOr }
    "&&" { const TokenLogicalAnd }
    "->" { const TokenArrow }
