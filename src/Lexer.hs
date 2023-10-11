module Lexer (
    lexer,
) where

import Errors
import Tokens

import Data.Char

lineNoStart :: Int
lineNoStart = 1
linePosStart :: Int
linePosStart = 1

lexer :: String -> ([Token], [LexerError])
lexer code = lexerHelper [] [] "" (code ++ ['\n']) lineNoStart linePosStart

lexerHelper :: [Token] -> [LexerError] -> String -> String -> Int -> Int -> ([Token], [LexerError])
lexerHelper finishedTokens errors currToken remainingStr lineNo linePos =
    case remainingStr of
        "" -> (finishedTokens, errors)
        '/' : '*' : leftover -> multilineComment finishedTokens errors leftover lineNo linePos lineNo linePos
        '/' : '/' : leftover -> oneLineComment finishedTokens errors leftover lineNo
        d : leftover
            | isDelim d ->
                let newLineNo = if d == '\n' then (lineNo + 1) else lineNo
                    newLinePos = if d == '\n' then linePosStart else (linePos + 1)
                    finishedToken = classifyToken currToken
                    delimToken = classifyDelim d (if (length leftover) == 0 then Nothing else Just (head leftover))
                    newRemainingStr = if (length delimToken) == 2 then (tail leftover) else leftover
                    tokensAddCurr =
                        if currToken == ""
                            then finishedTokens
                            else case finishedToken of
                                Just t -> finishedTokens ++ [t]
                                _ -> finishedTokens
                    newFinishedTokens =
                        case delimToken of
                            Just t -> tokensAddCurr ++ [t]
                            _ -> tokensAddCurr
                    newErrors =
                        if currToken == ""
                            then errors
                            else case finishedToken of
                                Nothing -> errors ++ [(LexerError lineNo (linePos - (length currToken)) InvalidTokenError)]
                                _ -> errors
                 in lexerHelper newFinishedTokens newErrors "" newRemainingStr newLineNo newLinePos
        d : leftover -> lexerHelper finishedTokens errors (currToken ++ [d]) leftover lineNo (linePos + 1)

oneLineComment :: [Token] -> [LexerError] -> String -> Int -> ([Token], [LexerError])
oneLineComment finishedTokens errors remainingStr lineNo =
    case remainingStr of
        "" -> (finishedTokens, errors)
        '\n' : _ -> lexerHelper finishedTokens errors "" (tail remainingStr) (lineNo + 1) linePosStart
        _ -> oneLineComment finishedTokens errors (tail remainingStr) lineNo

multilineComment :: [Token] -> [LexerError] -> String -> Int -> Int -> Int -> Int -> ([Token], [LexerError])
multilineComment finishedTokens errors remainingStr commentStartLineNo commentStartLinePos lineNo linePos =
    case remainingStr of
        "" -> (finishedTokens, (LexerError commentStartLineNo commentStartLinePos DanglingCommentError) : errors)
        '*' : '/' : leftover -> lexerHelper finishedTokens errors "" leftover lineNo (linePos + 2)
        '\n' : _ -> multilineComment finishedTokens errors (tail remainingStr) commentStartLineNo commentStartLinePos (lineNo + 1) linePosStart
        _ -> multilineComment finishedTokens errors (tail remainingStr) commentStartLineNo commentStartLinePos lineNo (linePos + 1)
