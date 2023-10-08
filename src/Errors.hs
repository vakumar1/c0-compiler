module Errors (
    LexerError(..),
    LexerErrorCategory(..),
) where

data LexerErrorCategory = InvalidTokenError | DanglingCommentError
instance Show LexerErrorCategory where
    show InvalidTokenError = "InvalidTokenError"
    show DanglingCommentError = "DanglingCommentError"

data LexerError = LexerError {
    lineNo :: Int,
    linePos :: Int,
    errorCat :: LexerErrorCategory
}
instance Show LexerError where
    show l = ((show . errorCat) l) ++ " -- lineNo=" ++ ((show . lineNo) l) ++ " linePos=" ++ ((show .linePos) l)


-- data InvalidTokenError = InvalidTokenError
--     {
--         lineNo :: Int,
--         linePos :: Int
--     }
-- instance Show InvalidTokenError where
--     show (InvalidTokenError lineNo linePos) = "InvalidTokenError -- lineNo=" ++ (show . lineNo) ++ " linePos=" ++ (show . linePos)

-- data DanglingCommentError = DanglingCommentError
--     {
--         lineNo :: Int,
--         linePos :: Int
--     }
-- instance Show DanglingCommentError where
--     show (DanglingCommentError lineNo linePos) = "DanglingCommentError -- lineNo=" ++ (show . lineNo) ++ " linePos=" ++ (show . linePos)
