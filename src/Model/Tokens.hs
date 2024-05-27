module Model.Tokens (
    Token (..),
    TokenCategory (..),
    TokenData (..),
) where

import Model.Types

data Token = Token
    { tokenCat :: TokenCategory
    , tokenData :: TokenData
    }
    deriving (Eq, Show)
data TokenCategory
    = 
    -- single-char delims 
    SEMICOLON
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
    | EXCL
    | TILDE
    | LEFT
    | RIGHT
    | AMP
    | CARET
    | PIPE
    | QUEST
    | COLON
    | COMMA
    -- double-char delims
    | LEFT_LEFT
    | RIGHT_RIGHT
    | LEFT_EQ
    | RIGHT_EQ
    | EQ_EQ
    | EXCL_EQ
    | AMP_AMP
    | PIPE_PIPE
    | PLUS_EQ
    | DASH_EQ
    | STAR_EQ
    | SLASH_EQ
    | PERC_EQ
    | AMP_EQ
    | CARET_EQ
    | PIPE_EQ
    | PLUS_PLUS
    | DASH_DASH
    -- three-char delims
    | LEFT_LEFT_EQ
    | RIGHT_RIGHT_EQ
    -- keywords/identifiers/numerals
    | IDENTIFIER String
    | DECNUM String
    | HEXNUM String
    | TYPE TypeCategory
    | IF
    | ELSE
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
    | TYPEDEF
    | EOF
    deriving (Eq, Show)
data TokenData = TokenData
    { tokenLineNo :: Int
    , tokenLinePos :: Int
    }
    deriving (Eq, Show)
