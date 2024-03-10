module Common.Errors (
    LexerError (..),
    LexerErrorCategory (..),
    ParserError (..),
    ParserErrorCategory (..),
    UseBeforeDeclarationError (..),
    DoubleDeclarationError (..),
    UseBeforeAssignmentError (..),
    OpTypeMismatch (..),
    AsnTypeMismatch (..),
    RetTypeMismatch (..),
    InvalidReturnError (..),
    VerificationError (..),
    compilerError,
) where

import Model.Tokens
import Model.Types

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

data UseBeforeAssignmentError = UseBeforeAssignmentError
    { useBeforeAssignmentErrorUse :: Token
    }
instance Show UseBeforeAssignmentError where
    show e =
        "UseBeforeAssignmentError"
            ++ " --\n"
            ++ " lineNo="
            ++ ((show . tokenLineNo . tokenData . useBeforeAssignmentErrorUse) e)
            ++ " linePos="
            ++ ((show . tokenLinePos . tokenData . useBeforeAssignmentErrorUse) e)
            ++ " token="
            ++ ((show . tokenCat . useBeforeAssignmentErrorUse) e)

data OpTypeMismatch = OpTypeMismatch
    { opTypeMismatchOp :: Token
    , opTypeMismatchArgTypes :: [TypeCategory]
    }
    deriving (Show)

data AsnTypeMismatch = AsnTypeMismatch
    { asnTypeMismatchVar :: Token
    , asnTypeMismatchAsnType :: TypeCategory
    , asnTypeMismatchExpType :: TypeCategory
    }
    deriving (Show)

-- TODO: add token identifying return statement location
data RetTypeMismatch = RetTypeMismatch
    { retTypeMismatchRetType :: TypeCategory
    , retTypeMismatchExpType :: TypeCategory
    }
    deriving (Show)

data InvalidReturnError = InvalidReturnError
    { invalidReturnErrorFn :: Token
    }
instance Show InvalidReturnError where
    show e =
        "InvalidReturnErr"
            ++ "\n"
            ++ "function -- "
            ++ (show (invalidReturnErrorFn e))

data VerificationError
    = USE_BEFORE_DECL UseBeforeDeclarationError
    | DOUBLE_DECL DoubleDeclarationError
    | USE_BEFORE_ASN UseBeforeAssignmentError
    | OP_TYPE_MISMATCH OpTypeMismatch
    | ASN_TYPE_MISMATCH AsnTypeMismatch
    | RET_TYPE_MISMATCH RetTypeMismatch
    | INVALID_RET InvalidReturnError
    deriving (Show)
