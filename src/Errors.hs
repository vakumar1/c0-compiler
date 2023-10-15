module Errors (
    LexerError (..),
    LexerErrorCategory (..),
    ParserError (..),
    ParserErrorCategory (..),
    compilerError,
) where

import Tokens

compilerError :: String -> String
compilerError msg = "[INTERNAL COMPILER ERROR OCCURRED.]: " ++ msg

data LexerErrorCategory = InvalidTokenError | DanglingCommentError
    deriving (Show)

data LexerError = LexerError
    { errorLineNo :: Int
    , errorLinePos :: Int
    , lexerErrorCat :: LexerErrorCategory
    }
instance Show LexerError where
    show l = ((show . lexerErrorCat) l) ++ " -- lineNo=" ++ ((show . errorLineNo) l) ++ " linePos=" ++ ((show . errorLinePos) l)

data ParserErrorCategory
    = EmptyExpression
    | DanglingOpenParen
    | DanglingCloseParen
    | DanglingUnaryOp
    | DanglingBinaryOp
    | ExpectedExpression
    | UnexpectedExpression
    | ExpectedSemicolon
    deriving (Show)

data ParserError = ParserError
    { parserErrorCat :: ParserErrorCategory
    , parserErrorToken :: Token
    }
instance Show ParserError where
    show p =
        ((show . parserErrorCat) p)
            ++ " --"
            ++ " lineNo="
            ++ ((show . tokenLineNo . tokenData . parserErrorToken) p)
            ++ " linePos="
            ++ ((show . tokenLinePos . tokenData . parserErrorToken) p)
            ++ " token="
            ++ ((show . tokenCat . parserErrorToken) p)
