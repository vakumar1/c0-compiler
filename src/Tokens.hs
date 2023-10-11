module Tokens (
    Token,
    TokenData,
    isDelim,
    classifyDelim,
    classifyToken,
) where

import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Data.Text as T

data Token
    = SEMICOLON
    | OPEN_PAREN
    | OPEN_BRACK
    | OPEN_BRACE
    | CLOSE_PAREN
    | CLOSE_BRACK
    | CLOSE_BRACE
    | PLUS
    | DASH
    | STAR
    | SLASH
    | PERC
    | PLUS_EQ
    | DASH_EQ
    | STAR_EQ
    | SLASH_EQ
    | PERC_EQ
    | EQUAL
    | IDENTIFIER String
    | DECNUM String
    | HEXNUM String
    | WHILE
    | FOR
    | CONTINUE
    | BREAK
    | RETURN
    | ASSERT
    | TRUE
    | FALSE
    | NULL
    | ALLOC
    | ALLOC_ARRAY
    | INT
    | BOOL
    | VOID
    | CHAR
    | STRING
    deriving (Show)
data TokenData = TokenData
    { token :: Token
    }
    deriving (Show)

whitespaceDelims :: String
whitespaceDelims = " \n\t\v\r\f"

reservedCharDelims :: String
reservedCharDelims = ";()[]{}+-*/%="

isDelim :: Char -> Bool
isDelim d = elem d whitespaceDelims || elem d reservedCharDelims

classifyDelim :: Char -> Maybe Char -> Maybe Token
classifyDelim d e = 
    let doubleTok = case e of
                        Just c -> reservedDoubleTok d c
                        Nothing -> Nothing
    in case doubleTok of
        Just t -> Just t
        _ -> reservedCharTok d

reservedDoubleTok :: Char -> Char -> Maybe Token
reservedDoubleTok d e = 
    case d:[e] of
        "+=" -> Just PLUS_EQ
        "-=" -> Just DASH_EQ
        "*=" -> Just STAR_EQ
        "/=" -> Just SLASH_EQ
        "%=" -> Just PERC_EQ
        _ -> Nothing

reservedCharTok :: Char -> Maybe Token
reservedCharTok d =
    case d of
        ';' -> Just SEMICOLON
        '(' -> Just OPEN_PAREN
        ')' -> Just CLOSE_PAREN
        '[' -> Just OPEN_BRACK
        ']' -> Just CLOSE_BRACK
        '{' -> Just OPEN_BRACE
        '}' -> Just CLOSE_BRACE
        '+' -> Just PLUS
        '-' -> Just DASH
        '*' -> Just STAR
        '/' -> Just SLASH
        '%' -> Just PERC
        '=' -> Just EQUAL
        _ -> Nothing

classifyToken :: String -> Maybe Token
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

reservedKeywordTok :: String -> Maybe Token
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

decnumTok :: String -> Maybe Token
decnumTok s =
    case s of
        "0" -> Just (DECNUM s)
        f : rest
            | (f /= '0')
                && (all C.isDigit s) ->
                Just (DECNUM s)
        _ -> Nothing

hexnumTok :: String -> Maybe Token
hexnumTok s =
    case s of
        '0' : x : rest
            | (x == 'x' || x == 'X')
                && (all (\c -> C.isDigit c || elem c "abcdefABCDEF") rest) ->
                Just (HEXNUM s)
        _ -> Nothing

idenifierTok :: String -> Maybe Token
idenifierTok s =
    let alphaUnder = \c -> C.isLetter c || c == '_'
     in case s of
            f : rest
                | (alphaUnder f)
                    && (all (\c -> alphaUnder c || C.isDigit c) s) ->
                    Just (IDENTIFIER s)
            _ -> Nothing
