module Errors (
    LexerError (..),
    LexerErrorCategory (..),
    ParserError (..),
    compilerError,
) where

compilerError :: String -> String
compilerError msg = "[INTERNAL COMPILER ERROR OCCURRED.]: " ++ msg

data LexerErrorCategory = InvalidTokenError | DanglingCommentError
    deriving (Show)

data LexerError = LexerError
    { errorLineNo :: Int
    , errorLinePos :: Int
    , errorCat :: LexerErrorCategory
    }
instance Show LexerError where
    show l = ((show . errorCat) l) ++ " -- lineNo=" ++ ((show . errorLineNo) l) ++ " linePos=" ++ ((show . errorLinePos) l)

data ParserError
    = EmptyExpressionError
    | DanglingOpenParen
    | DanglingUnaryOp
    | DanglingBinaryOp
    deriving (Show)

-- data ParserError = ParserError {
--     errorCat :: ParserErrorCategory
-- }
