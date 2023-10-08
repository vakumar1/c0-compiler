module Tokens (
    Token,
    TokenCategory,
    isDelim,
    reservedCharTok,
    classifyToken,
) where

import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Data.Text as T

data TokenCategory
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
    | EQUAL
    | IDENTIFIER
    | DECNUM
    | HEXNUM
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
data Token = Token
    { tokCat :: TokenCategory
    , tokValue :: Maybe String
    }

whitespaceDelims :: String
whitespaceDelims = " \n\t\v\r\f"

reservedCharDelims :: String
reservedCharDelims = ";()[]{}+-*/%="

isDelim :: Char -> Bool
isDelim d = elem d whitespaceDelims || elem d reservedCharDelims

reservedCharTok :: Char -> Maybe Token
reservedCharTok d =
    case d of
        ';' -> Just (Token SEMICOLON Nothing)
        '(' -> Just (Token OPEN_PAREN Nothing)
        ')' -> Just (Token CLOSE_PAREN Nothing)
        '[' -> Just (Token OPEN_BRACK Nothing)
        ']' -> Just (Token CLOSE_BRACK Nothing)
        '{' -> Just (Token OPEN_BRACE Nothing)
        '}' -> Just (Token CLOSE_BRACE Nothing)
        '+' -> Just (Token PLUS Nothing)
        '-' -> Just (Token DASH Nothing)
        '*' -> Just (Token STAR Nothing)
        '/' -> Just (Token SLASH Nothing)
        '%' -> Just (Token PERC Nothing)
        '=' -> Just (Token EQUAL Nothing)
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
        "while" -> Just (Token WHILE Nothing)
        "for" -> Just (Token FOR Nothing)
        "continue" -> Just (Token CONTINUE Nothing)
        "break" -> Just (Token BREAK Nothing)
        "return" -> Just (Token RETURN Nothing)
        "assert" -> Just (Token ASSERT Nothing)
        "true" -> Just (Token TRUE Nothing)
        "false" -> Just (Token FALSE Nothing)
        "NULL" -> Just (Token NULL Nothing)
        "alloc" -> Just (Token ALLOC Nothing)
        "alloc_array" -> Just (Token ALLOC_ARRAY Nothing)
        "int" -> Just (Token INT Nothing)
        "bool" -> Just (Token BOOL Nothing)
        "void" -> Just (Token VOID Nothing)
        "char" -> Just (Token CHAR Nothing)
        "string" -> Just (Token STRING Nothing)
        _ -> Nothing

decnumTok :: String -> Maybe Token
decnumTok s =
    case s of
        "0" -> Just (Token DECNUM (Just s))
        f : rest
            | (f /= '0')
                && (all C.isDigit rest) ->
                Just (Token DECNUM (Just s))
        _ -> Nothing

hexnumTok :: String -> Maybe Token
hexnumTok s =
    case s of
        '0' : x : rest
            | (x == 'x' || x == 'X')
                && (all (\c -> C.isDigit c || elem c "abcdefABCDEF") s) ->
                Just (Token DECNUM (Just s))
        _ -> Nothing

idenifierTok :: String -> Maybe Token
idenifierTok s =
    let alphaUnder = \c -> C.isLetter c || c == '_'
     in case s of
            f : rest
                | (alphaUnder f)
                    && (all (\c -> alphaUnder c || C.isDigit c) s) ->
                    Just (Token IDENTIFIER (Just s))
            _ -> Nothing
