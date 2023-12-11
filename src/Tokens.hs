module Tokens (
    Token (..),
    TokenCategory (..),
    TokenData (..),
) where

data Token = Token
    { tokenCat :: TokenCategory
    , tokenData :: TokenData
    }
    deriving (Eq, Show)
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
    deriving (Eq, Show)
