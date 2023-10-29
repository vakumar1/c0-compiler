module Lexer (
    lexer,
) where

import Errors
import Tokens

import qualified Data.Char as C
import qualified Data.Foldable as F

lineNoStart :: Int
lineNoStart = 1
linePosStart :: Int
linePosStart = 1

lexer :: String -> ([Token], [LexerError])
lexer code = lexerHelper [] [] [] "" (code ++ ['\n']) lineNoStart linePosStart

lexerHelper :: [Token] -> [Token] -> [LexerError] -> String -> String -> Int -> Int -> ([Token], [LexerError])
lexerHelper finishedTokens openerStack errors currToken remainingStr lineNo linePos =
    case remainingStr of
        -- EOF
        "" -> 
            let errorsAddDangler = foldr (\o l -> (LexerError ((tokenLineNo . tokenData) o) ((tokenLinePos . tokenData) o) DanglingOpenEncloserError):l) errors openerStack
            in ((Token EOF (TokenData lineNo linePos)):finishedTokens, errorsAddDangler)

        -- TODO: move under delimiters (to wrap up currToken)
        -- comment begin
        '/' : '*' : _ -> 
            multilineComment finishedTokens openerStack errors (drop 2 remainingStr) lineNo linePos lineNo linePos
        '/' : '/' : _ -> 
            oneLineComment finishedTokens openerStack errors (drop 2 remainingStr) lineNo

        -- delimiters

        d : _ | isDelim d ->
            let (delimTokenCat, delimLength) = case classifyDelim remainingStr of
                    Just (c, l) -> (Just c, l)
                    Nothing -> (Nothing, 1)
                newRemainingStr = (drop delimLength remainingStr)
                finishedTokenCat = classifyToken currToken
                tokensAddCurr =
                    if currToken == ""
                        then finishedTokens
                        else case finishedTokenCat of
                            Just t -> (Token t (TokenData lineNo (linePos - (length currToken)))):finishedTokens
                            _ -> finishedTokens
                newFinishedTokens =
                    case delimTokenCat of
                        Just c -> (Token c (TokenData lineNo linePos)):tokensAddCurr
                        Nothing -> tokensAddCurr
                newErrors =
                    if currToken == ""
                        then errors
                        else case finishedTokenCat of
                            Nothing -> ((LexerError lineNo (linePos - (length currToken)) InvalidTokenError)):errors
                            _ -> errors
            in case d of
                '\n' -> 
                    lexerHelper newFinishedTokens openerStack newErrors "" newRemainingStr (lineNo + 1) 0
                _ | elem d whitespaceDelims -> 
                    lexerHelper newFinishedTokens openerStack newErrors "" newRemainingStr lineNo (linePos + 1)
                _ | elem d "{[(" ->
                    lexerHelper newFinishedTokens ((head newFinishedTokens):openerStack) newErrors "" newRemainingStr lineNo (linePos + 1)
                _ | elem d "}])" ->
                    if closerMatchesLastOpener openerStack d
                        then 
                            let haltCat = case d of
                                            ')' -> OPEN_PAREN
                                            '}' -> OPEN_BRACE
                                            ']' -> OPEN_BRACK
                                collapsedFinishedTokens = collapseEnclosing haltCat newFinishedTokens
                            in lexerHelper collapsedFinishedTokens (tail openerStack) newErrors "" newRemainingStr lineNo (linePos + 1)
                        else 
                            let errorsAddDangler = ((LexerError lineNo linePos DanglingClosedEncloserError):newErrors)
                            in lexerHelper (tail newFinishedTokens) openerStack errorsAddDangler "" newRemainingStr lineNo (linePos + 1)
                _ ->
                    lexerHelper newFinishedTokens openerStack newErrors "" newRemainingStr lineNo (linePos + delimLength)

        -- general case
        d : _ ->
            lexerHelper finishedTokens openerStack errors (currToken ++ [d]) (tail remainingStr) lineNo (linePos + 1)


-- COMMENT HELPERS

oneLineComment :: [Token] -> [Token] -> [LexerError] -> String -> Int -> ([Token], [LexerError])
oneLineComment finishedTokens openerStack errors remainingStr lineNo =
    case remainingStr of
        "" -> 
            lexerHelper finishedTokens openerStack errors "" (tail remainingStr) (lineNo + 1) linePosStart
        '\n' : _ -> 
            lexerHelper finishedTokens openerStack errors "" (tail remainingStr) (lineNo + 1) linePosStart
        _ -> 
            oneLineComment finishedTokens openerStack errors (tail remainingStr) lineNo

multilineComment :: [Token] -> [Token] -> [LexerError] -> String -> Int -> Int -> Int -> Int -> ([Token], [LexerError])
multilineComment finishedTokens openerStack errors remainingStr commentStartLineNo commentStartLinePos lineNo linePos =
    case remainingStr of
        "" -> 
            let errorsAddDangler = ((LexerError commentStartLineNo commentStartLinePos DanglingCommentError) : errors)
            in lexerHelper finishedTokens openerStack errorsAddDangler "" remainingStr lineNo linePos
        '*' : '/' : leftover -> 
            lexerHelper finishedTokens openerStack errors "" leftover lineNo (linePos + 2)
        '\n' : _ -> 
            multilineComment finishedTokens openerStack errors (tail remainingStr) commentStartLineNo commentStartLinePos (lineNo + 1) linePosStart
        _ -> 
            multilineComment finishedTokens openerStack errors (tail remainingStr) commentStartLineNo commentStartLinePos lineNo (linePos + 1)


-- ENCLOSING (){}[] HELPERS

collapseEnclosing :: TokenCategory -> [Token] -> [Token]
collapseEnclosing haltCat tokens = collapseEnclosingHelper haltCat tokens []

collapseEnclosingHelper :: TokenCategory -> [Token] -> [Token] -> [Token]
collapseEnclosingHelper haltCat remainingTokens subTokens = 
    if haltCat == (tokenCat . head) remainingTokens
        then (Token (ENCLOSED_TOKS (head remainingTokens) subTokens) ((tokenData . head) remainingTokens)):(tail remainingTokens)
        else collapseEnclosingHelper haltCat (tail remainingTokens) ((head remainingTokens):subTokens)

closerMatchesLastOpener :: [Token] -> Char -> Bool
closerMatchesLastOpener openerStack closer = 
    if null openerStack
        then False
        else case (tokenCat . head) openerStack of
            OPEN_BRACE -> closer == '}'
            OPEN_PAREN -> closer == ')'
            OPEN_BRACK -> closer == ']'

-- DELIM HELPERS

whitespaceDelims :: String
whitespaceDelims = " \n\t\v\r\f"

reservedCharDelims :: String
reservedCharDelims = ";()[]{}+-*/%="

isDelim :: Char -> Bool
isDelim d = elem d whitespaceDelims || elem d reservedCharDelims

classifyDelim :: String -> Maybe (TokenCategory, Int)
classifyDelim remainingStr =
    F.asum
        [ case reservedDoubleTok remainingStr of
            Just t -> Just (t, 2)
            _ -> Nothing
        , case reservedCharTok remainingStr of
            Just t -> Just (t, 1)
            _ -> Nothing
        ]

reservedDoubleTok :: String -> Maybe TokenCategory
reservedDoubleTok s =
    case s of
        '+' : '=' : _ -> Just PLUS_EQ
        '-' : '=' : _ -> Just DASH_EQ
        '*' : '=' : _ -> Just STAR_EQ
        '/' : '=' : _ -> Just SLASH_EQ
        '%' : '=' : _ -> Just PERC_EQ
        _ -> Nothing

reservedCharTok :: String -> Maybe TokenCategory
reservedCharTok s =
    case s of
        ';' : _ -> Just SEMICOLON
        '(' : _ -> Just OPEN_PAREN
        ')' : _ -> Just CLOSE_PAREN
        '[' : _ -> Just OPEN_BRACK
        ']' : _ -> Just CLOSE_BRACK
        '{' : _ -> Just OPEN_BRACE
        '}' : _ -> Just CLOSE_BRACE
        '+' : _ -> Just PLUS
        '-' : _ -> Just DASH
        '*' : _ -> Just STAR
        '/' : _ -> Just SLASH
        '%' : _ -> Just PERC
        '=' : _ -> Just EQUAL
        _ -> Nothing

classifyToken :: String -> Maybe TokenCategory
classifyToken s =
    F.asum
        [ case reservedKeywordTok s of
            Just t -> Just t
            _ -> Nothing
        , case decnumTok s of
            Just t -> Just t
            _ -> Nothing
        , case hexnumTok s of
            Just t -> Just t
            _ -> Nothing
        , case idenifierTok s of
            Just t -> Just t
            _ -> Nothing
        ]

reservedKeywordTok :: String -> Maybe TokenCategory
reservedKeywordTok s =
    case s of
        "while" -> Just WHILE
        "for" -> Just FOR
        "continue" -> Just CONTINUE
        "break" -> Just BREAK
        "return" -> Just RETURN
        "assert" -> Just ASSERT
        "true" -> Just TRUE
        "false" -> Just FALSE
        "NULL" -> Just NULL
        "alloc" -> Just ALLOC
        "alloc_array" -> Just ALLOC_ARRAY
        "int" -> Just INT
        "bool" -> Just BOOL
        "void" -> Just VOID
        "char" -> Just CHAR
        "string" -> Just STRING
        _ -> Nothing

decnumTok :: String -> Maybe TokenCategory
decnumTok s =
    case s of
        "0" -> Just (DECNUM s)
        f : _
            | (f /= '0')
                && (all C.isDigit s) ->
                Just (DECNUM s)
        _ -> Nothing

hexnumTok :: String -> Maybe TokenCategory
hexnumTok s =
    case s of
        '0' : x : rest
            | (x == 'x' || x == 'X')
                && (all (\c -> C.isDigit c || elem c "abcdefABCDEF") rest) ->
                Just (HEXNUM s)
        _ -> Nothing

idenifierTok :: String -> Maybe TokenCategory
idenifierTok s =
    let alphaUnder = \c -> C.isLetter c || c == '_'
     in case s of
            f : _
                | (alphaUnder f)
                    && (all (\c -> alphaUnder c || C.isDigit c) s) ->
                    Just (IDENTIFIER s)
            _ -> Nothing
