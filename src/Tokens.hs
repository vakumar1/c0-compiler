module Tokens (
    Token (..),
    TokenCategory (..),
    TokenData (..),
    isDelim,
    classifyDelim,
    classifyToken,
) where

import qualified Data.Char as C
import qualified Data.Foldable as F

data Token = Token
    { tokenCat :: TokenCategory
    , tokenData :: TokenData
    }
    deriving (Show)
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
    | EOF
    deriving (Eq, Show)
data TokenData = TokenData
    { tokenLineNo :: Int
    , tokenLinePos :: Int
    }
    deriving (Show)

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
