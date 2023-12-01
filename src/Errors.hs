module Errors (
    LexerError (..),
    LexerErrorCategory (..),
    ParserError (..),
    ParserErrorCategory (..),
    UseBeforeDeclarationError (..),
    DoubleDeclarationError (..),
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

data UseBeforeDeclarationError = UseBeforeDeclarationError
    { useBeforeInitializationErrorUse :: Token
    }
instance Show UseBeforeDeclarationError where
    show e =
        "UseBeforeDeclarationError"
            ++ " --\n"
            ++ " lineNo="
            ++ ((show . tokenLineNo . tokenData . useBeforeInitializationErrorUse) e)
            ++ " linePos="
            ++ ((show . tokenLinePos . tokenData . useBeforeInitializationErrorUse) e)
            ++ " token="
            ++ ((show . tokenCat . useBeforeInitializationErrorUse) e)

data DoubleDeclarationError = DoubleDeclarationError
    { doubleInitializationErrorFirstInit :: Token
    , doubleInitializationErrorSecondInit :: Token
    }
instance Show DoubleDeclarationError where
    show e =
        "DoubleDeclarationError"
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

data InvalidReturnError = InvalidReturnError
    { invalidReturnStmt :: Token
    }

data VerificationError
    = USE_BEFORE_DECL UseBeforeDeclarationError
    | DOUBLE_DECL DoubleDeclarationError
    deriving (Show)
