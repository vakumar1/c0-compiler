module Lexer (
    lexer,
) where

import Tokens
import Errors

import Data.Char

lexer :: String -> ([Token], [LexerError])
lexer code = lexerHelper [] [] "" code 0 0


lexerHelper :: [Token] -> [LexerError] -> String -> String -> Int -> Int -> ([Token], [LexerError])
lexerHelper finishedTokens errors currToken remainingStr lineNo linePos = 
    case remainingStr of
        ""                  -> (finishedTokens, errors)
        '/':'*':leftover    -> multilineComment finishedTokens errors remainingStr lineNo linePos lineNo linePos
        '/':'/':leftover    -> oneLineComment finishedTokens errors remainingStr lineNo
        '\n':_              -> lexerHelper finishedTokens errors currToken (tail remainingStr) (lineNo + 1) 0
        _                   -> lexerHelper finishedTokens errors currToken (tail remainingStr) lineNo (linePos + 1)


oneLineComment :: [Token] -> [LexerError] -> String -> Int -> ([Token], [LexerError])
oneLineComment finishedTokens errors remainingStr lineNo = 
    case remainingStr of
        ""      -> (finishedTokens, errors)
        '\n':_  -> lexerHelper finishedTokens errors "" (tail remainingStr) (lineNo + 1) 0
        _       -> oneLineComment finishedTokens errors (tail remainingStr) lineNo


multilineComment :: [Token] -> [LexerError] -> String -> Int -> Int -> Int -> Int -> ([Token], [LexerError])
multilineComment finishedTokens errors remainingStr commentStartLineNo commentStartLinePos lineNo linePos = 
    case remainingStr of
        ""                  -> (finishedTokens, (LexerError commentStartLineNo commentStartLinePos DanglingCommentError):errors)
        '*':'/':leftover    -> lexerHelper finishedTokens errors "" leftover lineNo (linePos + 2)
        '\n':_              -> multilineComment finishedTokens errors (tail remainingStr) commentStartLineNo commentStartLinePos (lineNo + 1) 0
        _                   -> multilineComment finishedTokens errors (tail remainingStr) commentStartLineNo commentStartLinePos lineNo (linePos + 1)
