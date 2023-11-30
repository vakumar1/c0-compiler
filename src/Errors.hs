module Errors (
    LexerError (..),
    LexerErrorCategory (..),
    ParserError (..),
    ParserErrorCategory (..),
    UseBeforeInitializationError (..),
    DoubleInitializationError (..),
    VerificationError (..),
    compilerError,
) where

import Tokens

compilerError :: String -> String
compilerError msg = "[INTERNAL COMPILER ERROR OCCURRED.]: " ++ msg

data LexerErrorCategory
    = InvalidTokenError
    | DanglingCommentError
    | DanglingOpenEncloserError
    | DanglingClosedEncloserError
    deriving (Show)

data LexerError = LexerError
    { errorLineNo :: Int
    , errorLinePos :: Int
    , lexerErrorCat :: LexerErrorCategory
    }
instance Show LexerError where
    show l = ((show . lexerErrorCat) l) ++ " -- lineNo=" ++ ((show . errorLineNo) l) ++ " linePos=" ++ ((show . errorLinePos) l)

data ParserErrorCategory
    = -- EXP ERRORS
      EmptyExpression
    | DanglingOpenParen
    | DanglingCloseParen
    | DanglingUnaryOp
    | DanglingBinaryOp
    | ExpectedExpression
    | UnexpectedExpression
    | UnexpectedTokenInExpression
    | -- STMT ERRORS
      ExpectedSemicolon
    | DanglingType
    | UnexpectedType
    | UnexpectedIdentifier
    | UnexpectedReturn
    | UnexpectedASNOp
    | UnexpectedTokenInStatement
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

data UseBeforeInitializationError = UseBeforeInitializationError
    { useBeforeInitializationErrorUse :: Token
    }
instance Show UseBeforeInitializationError where
    show e =
        "UseBeforeInitializationError"
            ++ " --\n"
            ++ " lineNo="
            ++ ((show . tokenLineNo . tokenData . useBeforeInitializationErrorUse) e)
            ++ " linePos="
            ++ ((show . tokenLinePos . tokenData . useBeforeInitializationErrorUse) e)
            ++ " token="
            ++ ((show . tokenCat . useBeforeInitializationErrorUse) e)

data DoubleInitializationError = DoubleInitializationError
    { doubleInitializationErrorFirstInit :: Token
    , doubleInitializationErrorSecondInit :: Token
    }
instance Show DoubleInitializationError where
    show e =
        "DoubleInitializationError"
            ++ "\n"
            ++ " firstInit --"
            ++ " lineNo="
            ++ ((show . tokenLineNo . tokenData . doubleInitializationErrorFirstInit) e)
            ++ " linePos="
            ++ ((show . tokenLinePos . tokenData . doubleInitializationErrorFirstInit) e)
            ++ " token="
            ++ ((show . tokenCat . doubleInitializationErrorFirstInit) e)
            ++ " \n"
            ++ " secondInit --"
            ++ " lineNo="
            ++ ((show . tokenLineNo . tokenData . doubleInitializationErrorSecondInit) e)
            ++ " linePos="
            ++ ((show . tokenLinePos . tokenData . doubleInitializationErrorSecondInit) e)
            ++ " token="
            ++ ((show . tokenCat . doubleInitializationErrorSecondInit) e)

data VerificationError
    = USE_BEFORE_INIT UseBeforeInitializationError
    | DOUBLE_INIT DoubleInitializationError
    deriving (Show)
