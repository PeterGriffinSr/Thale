{
{-# LANGUAGE Unsafe #-}
module Thale.Compiler.Lexer (alexScanTokens) where

import Thale.Compiler.Token (Token(..), LocatedToken(..))
import Control.Exception (try, evaluate, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitFailure)
import Thale.Compiler.Error (LexError(..), printLexError)
import Thale.Util.Source (setSource)
import Prelude (Bool (..), Char, Either (..), Eq, IO, Maybe (..), Ord, Show, String, error, fromIntegral, length, map, mod, otherwise, pure, read, seq, show, take, uncurry, ($), (&&), (+), (++), (-), (.), (<), (<=), (>=), (||))
}

%wrapper "posn"

tokens :-

    $white+ ;
    "#".* ;
    "#*" (.| \n )* "*#" ;

    [0-9]+"."[0-9]+ { \pos s -> LocatedToken (alexLine pos) (alexColumn pos) (TokenFloat (read s)) }
    [0-9]+ { \pos s -> LocatedToken (alexLine pos) (alexColumn pos) (TokenFloat (read s)) }
    \"([^\\\"]|\\[nrt\\\"\'])*\" {
        \pos s -> LocatedToken (alexLine pos) (alexColumn pos) (TokenString (read s))
    }
    \"([^\\\"]|\\.)* {
        \pos s ->
            let endCol = alexColumn pos + length s
            in alexError (LexError "Unterminated string literal" (alexLine pos) endCol s)
    }
    \"([^\\\"]|\\.)*\\[^nrt\\\"\']([^\\\"]|\\.)*\"? {
        \pos s ->
            let endCol = alexColumn pos + length s
            in alexError (LexError "Invalid escape sequence in string literal" (alexLine pos) endCol s)
    }
    \'([^\\\']|\\[nrt\\\"\'])\' {
        \pos s -> LocatedToken (alexLine pos) (alexColumn pos) (TokenChar (read s))
    }
    \'([^\\\']|\\.)? {
        \pos s ->
            let endCol = alexColumn pos + length s
            in alexError (LexError "Unterminated character literal" (alexLine pos) endCol s)
    }
    \'([^\\\']|\\.)\'? {
        \pos s ->
            let endCol = alexColumn pos + length s
            in alexError (LexError "Invalid escape sequence in character literal" (alexLine pos) endCol s)
    }

    [a-zA-Z][a-zA-Z0-9\'_]* {
        \pos s ->
          let tok = case s of
                "val"   -> TokenVal
                "type"  -> TokenType
                "use"   -> TokenUse
                "match" -> TokenMatch
                "with"  -> TokenWith
                "Float" -> TokenFloatType
                "Char"  -> TokenCharType
                "Unit"  -> TokenUnitType
                "Bool"  -> TokenBoolType
                "List"  -> TokenListType
                "True"  -> TokenBool True
                "False" -> TokenBool False
                _       -> TokenIdentifier s
          in LocatedToken (alexLine pos) (alexColumn pos) tok
    }

    "(" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenLParen }
    ")" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenRParen }
    "[" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenLBracket }
    "]" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenRBracket }
    "{" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenLBrace }
    "}" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenRBrace }
    "+" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenPlus }
    "-" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenMinus }
    "*" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenStar }
    "/" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenSlash }
    "%" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenPercent }
    "^" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenCarot }
    "<" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenLess }
    ">" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenGreater }
    "!" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenBang }
    ":" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenColon }
    "," { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenComma }
    "|" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenPipe }
    "." { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenDot }
    "=" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenEqual }
    "_" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenUnderscore }
    ";" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenSemicolon }
    "<>" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenNotEqual }
    "||" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenLogicalOr }
    "&&" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenLogicalAnd }
    "->" { \pos _ -> LocatedToken (alexLine pos) (alexColumn pos) TokenArrow }

    . {
        \pos s ->
            alexError (LexError
                ("Unexpected character: " ++ show s)
                (alexLine pos)
                (alexColumn pos)
                s)
    }

{
alexLine :: AlexPosn -> Int
alexLine (AlexPn _ line _) = line

alexColumn :: AlexPosn -> Int
alexColumn (AlexPn _ _ col) = col

alexError :: LexError -> a
alexError err = unsafePerformIO $ do
  printLexError err
  exitFailure

alexScanTokensWithErrors :: String -> Either LexError [LocatedToken]
alexScanTokensWithErrors input = unsafePerformIO $ do
  setSource input
  r <- try (evaluate (alexScanTokens input)) :: IO (Either SomeException [LocatedToken])
  case r of
    Left e  -> pure (Left (LexError (show e) 0 0 ""))
    Right v -> pure (Right v)
}
