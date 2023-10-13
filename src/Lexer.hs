module Lexer (
    lexer,
) where

import Errors
import Tokens

lineNoStart :: Int
lineNoStart = 1
linePosStart :: Int
linePosStart = 1

lexer :: String -> ([Token], [LexerError])
lexer code = lexerHelper [] [] "" (code ++ ['\n']) lineNoStart linePosStart

lexerHelper :: [Token] -> [LexerError] -> String -> String -> Int -> Int -> ([Token], [LexerError])
lexerHelper finishedTokens errors currToken remainingStr lineNo linePos =
    case remainingStr of
        "" -> (finishedTokens ++ [Token EOF (TokenData lineNo linePos)], errors)
        '/' : '*' : leftover -> multilineComment finishedTokens errors leftover lineNo linePos lineNo linePos
        '/' : '/' : leftover -> oneLineComment finishedTokens errors leftover lineNo
        d : leftover
            | isDelim d ->
                let (delimTokenCat, delimTokenLength) = case (classifyDelim remainingStr) of
                        Just (c, l) -> (Just c, l)
                        Nothing -> (Nothing, 1)
                    newLineNo = if d == '\n' then (lineNo + 1) else lineNo
                    newLinePos = if d == '\n' then linePosStart else (linePos + delimTokenLength)
                    newRemainingStr = drop delimTokenLength remainingStr
                    finishedTokenCat = classifyToken currToken
                    tokensAddCurr =
                        if currToken == ""
                            then finishedTokens
                            else case finishedTokenCat of
                                Just t -> finishedTokens ++ [Token t (TokenData lineNo (linePos - (length currToken)))]
                                _ -> finishedTokens
                    newFinishedTokens =
                        case delimTokenCat of
                            Just c -> tokensAddCurr ++ [Token c (TokenData lineNo linePos)]
                            Nothing -> tokensAddCurr
                    newErrors =
                        if currToken == ""
                            then errors
                            else case finishedTokenCat of
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
