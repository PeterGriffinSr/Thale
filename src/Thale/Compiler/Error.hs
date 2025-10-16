{-# LANGUAGE LambdaCase #-}

module Thale.Compiler.Error
  ( LexError (..),
    printLexError,
    printParseError,
  )
where

import Control.Applicative ((<|>))
import Data.IORef (readIORef)
import Data.List (find, isSuffixOf)
import GHC.IO (unsafePerformIO)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)
import Thale.Compiler.Token
  ( LocatedToken (..),
    Token (..),
    tokenToString,
  )
import Thale.Util.Color (Color (..), colorize, enableVirtualTerminal)
import Thale.Util.Source (sourceRef)

data LexError = LexError
  { lexErrorMessage :: String,
    lexErrorLine :: !Int,
    lexErrorColumn :: !Int,
    lexErrorSnippet :: String
  }
  deriving (Show, Eq)

printLexError :: LexError -> IO ()
printLexError (LexError msg line col snippet) = do
  enableVirtualTerminal
  src <- readIORef sourceRef
  let srcLines = lines src
      offendingLine =
        if line > 0 && line <= length srcLines
          then srcLines !! (line - 1)
          else snippet
      pointer = replicate (max 0 (col - 1)) ' ' ++ "^"

  hPutStrLn stderr $ colorize Red "error:" ++ " " ++ msg
  hPutStrLn stderr $
    colorize Cyan (" --> line " ++ show line ++ ", column " ++ show col)
  hPutStrLn stderr "  |"
  hPutStrLn stderr $ colorize Reset (show line ++ " | " ++ offendingLine)
  hPutStrLn stderr $ colorize Yellow ("  | " ++ pointer)
  hFlush stderr

printParseError :: [LocatedToken] -> a
printParseError toks = unsafePerformIO $ do
  enableVirtualTerminal
  source <- readIORef sourceRef
  let srcLines = lines source
      maxLookback = 5
      operators =
        [ ('=', TokenEqual),
          ('+', TokenPlus),
          ('-', TokenMinus),
          ('*', TokenStar),
          ('/', TokenSlash),
          ('%', TokenPercent),
          ('<', TokenLess),
          ('>', TokenGreater)
        ]

      multiOperators =
        [ ("<>", TokenNotEqual),
          ("||", TokenLogicalOr),
          ("&&", TokenLogicalAnd)
        ]

      findDanglingOp :: Int -> Maybe (Token, Int, Int)
      findDanglingOp startLine =
        let candidateLines = take maxLookback [startLine, startLine - 1 .. 1]

            checkLine ln
              | ln <= 0 || ln > length srcLines = Nothing
              | otherwise =
                  let raw = srcLines !! (ln - 1)
                      trimmed = reverse (dropWhile (`elem` [' ', '\t']) (reverse raw))
                   in if null trimmed
                        then Nothing
                        else
                          let foundMulti =
                                foldr
                                  ( \(opStr, tok) acc ->
                                      case acc of
                                        Just _ -> acc
                                        Nothing ->
                                          if opStr `isSuffixOf` trimmed
                                            then Just (tok, ln, length trimmed - length opStr + 1)
                                            else Nothing
                                  )
                                  Nothing
                                  multiOperators
                           in case foundMulti of
                                Just res -> Just res
                                Nothing ->
                                  let lastChar = last trimmed
                                      idxs = [i | (i, ch) <- zip [0 ..] trimmed, ch == lastChar]
                                   in case lookup lastChar operators of
                                        Just tok ->
                                          case idxs of
                                            [] -> Nothing
                                            _ -> let colIdx = last idxs + 1 in Just (tok, ln, colIdx)
                                        Nothing -> Nothing
         in foldr (\ln acc -> acc <|> checkLine ln) Nothing candidateLines

      tokenToReport :: LocatedToken
      tokenToReport =
        case find isClosing toks of
          Just t -> t
          Nothing ->
            case findUnmatchedOpening toks of
              Just t -> t
              Nothing -> case toks of
                [] -> LocatedToken 0 0 TokenEof
                (t : _) -> t
        where
          isClosing (LocatedToken _ _ t) = t `elem` [TokenRBracket, TokenRBrace, TokenRParen]

          findUnmatchedOpening :: [LocatedToken] -> Maybe LocatedToken
          findUnmatchedOpening ts =
            let pairs =
                  [ (TokenLParen, TokenRParen),
                    (TokenLBrace, TokenRBrace),
                    (TokenLBracket, TokenRBracket)
                  ]

                closingToOpening = [(b, a) | (a, b) <- pairs]

                go [] [] = Nothing
                go [] (unmatched : _) = Just unmatched
                go (LocatedToken l c t : rest) stack
                  | Just _ <- lookup t pairs =
                      go rest (LocatedToken l c t : stack)
                  | Just _ <- lookup t closingToOpening =
                      case stack of
                        (top@(LocatedToken _ _ openTok) : xs)
                          | lookup openTok pairs == Just t ->
                              go rest xs
                          | otherwise ->
                              Just top
                        [] ->
                          Nothing
                  | otherwise =
                      go rest stack
             in go ts []

      (tokStr, lineNum, colNum, hint) =
        case tokenToReport of
          LocatedToken l c t ->
            case findDanglingOp (l - 1) of
              Just (tok', ln, coln) -> (tokenToString tok', ln, coln, hintFor tok')
              Nothing -> (tokenToString t, l, c, hintFor t)

      offendingLine =
        if lineNum > 0 && lineNum <= length srcLines then srcLines !! (lineNum - 1) else ""

      maxLen = 80
      (displayLine, displayPointer) =
        if length offendingLine > maxLen
          then
            let start = max 0 (colNum - 40)
                end = min (length offendingLine) (start + maxLen)
                slice = take (end - start) (drop start offendingLine)
                pointer = replicate (colNum - start - 1) ' ' ++ "^"
             in (slice, pointer)
          else (offendingLine, replicate (colNum - 1) ' ' ++ "^")

      lineNumberWidth = length (show lineNum)
      pad = replicate lineNumberWidth ' '

  hPutStr stderr $ colorize Red "error:"
  hPutStrLn stderr $ colorize Reset (" Unexpected token: \"" ++ tokStr ++ "\"")
  hPutStrLn stderr $ colorize Cyan (" --> line " ++ show lineNum ++ ", column " ++ show colNum)
  hPutStrLn stderr $ colorize Reset (pad ++ " |")
  hPutStrLn stderr $ colorize Reset (show lineNum ++ " | " ++ displayLine)
  hPutStrLn stderr $ colorize Yellow (pad ++ " | " ++ displayPointer)
  hPutStrLn stderr $ colorize Magenta ("note: " ++ hint)
  hFlush stderr
  exitFailure

hintFor :: Token -> String
hintFor = \case
  TokenPlus -> "Operator '+' must be followed by an expression."
  TokenMinus -> "Operator '-' must be followed by an expression."
  TokenStar -> "Operator '*' must be between two expressions."
  TokenSlash -> "Operator '/' must be between two expressions."
  TokenPercent -> "Operator '%' must be between two expressions."
  TokenEqual -> "Assignment '=' must have a value after it."
  TokenArrow -> "Arrow '->' must be followed by a type or expression."
  TokenPipe -> "Unexpected '|'. Maybe you started a match case without 'match'?"
  TokenNotEqual -> "Operator '<>' must compare two expressions."
  TokenLogicalAnd -> "Operator '&&' must be between two boolean expressions."
  TokenLogicalOr -> "Operator '||' must be between two boolean expressions."
  TokenRBrace -> "Unexpected '}'. No matching '{' found."
  TokenRParen -> "Unexpected ')'. Likely missing an expression before this."
  TokenRBracket -> "Unexpected ']'. No matching '[' found."
  _ -> "Unexpected token or malformed expression."