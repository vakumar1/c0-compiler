module Errors (
    LexerError(..),
    LexerErrorCategory(..),
) where

data LexerErrorCategory = InvalidTokenError | DanglingCommentError
    deriving (Show)
-- instance Show LexerErrorCategory where
--     show InvalidTokenError = "InvalidTokenError"
--     show DanglingCommentError = "DanglingCommentError"

data LexerError = LexerError {
    lineNo :: Int,
    linePos :: Int,
    errorCat :: LexerErrorCategory
}
instance Show LexerError where
    show l = ((show . errorCat) l) ++ " -- lineNo=" ++ ((show . lineNo) l) ++ " linePos=" ++ ((show .linePos) l)
