module Common.Errors (
    LexerErrorCategory (..),
    LexerError (..),
    UseBeforeDeclarationError (..),
    DoubleDeclarationError (..),
    UseBeforeAssignmentError (..),
    OpTypeMismatch (..),
    AsnTypeMismatch (..),
    IfCondTypeMismatch (..),
    RetTypeMismatch (..),
    InvalidReturnError (..),
    DuplicateFnDefnError (..),
    VerificationError (..),
    compilerError,
) where

import Model.Tokens
import Model.Types

compilerError :: String -> String
compilerError msg = "[INTERNAL COMPILER ERROR OCCURRED.]: " ++ msg

data NonexistentTypeError = NonexistentTypeError
    { nonexistentTypeAlias :: Token
    }
    deriving (Show)

data CircularTypeResolutionError = CircularTypeResolutionError
    { circularTypeResolutionAlias :: Token
    }
    deriving (Show)

data LexerErrorCategory
    = INVALID_TOKEN
    | DANGLING_COMMENT
    | DANGLING_OPEN_ENCLOSER
    | DANGLING_CLOSED_ENCLOSER
    | NONEXISTENT_TYPE NonexistentTypeError
    | CIRCULAR_TYPE_RESOLUTION CircularTypeResolutionError
    deriving (Show)

data LexerError = LexerError
    { errorLineNo :: Int
    , errorLinePos :: Int
    , lexerErrorCat :: LexerErrorCategory
    }
    deriving (Show)

data UseBeforeDeclarationError = UseBeforeDeclarationError
    { useBeforeInitializationErrorUse :: Token
    }
    deriving (Show)

data DoubleDeclarationError = DoubleDeclarationError
    { doubleInitializationErrorFirstInit :: Token
    , doubleInitializationErrorSecondInit :: Token
    }
    deriving (Show)

data UseBeforeAssignmentError = UseBeforeAssignmentError
    { useBeforeAssignmentErrorUse :: Token
    }
    deriving (Show)

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

data IfCondTypeMismatch = IfCondTypeMismatch
    { ifCondTypeMismatchExpType :: TypeCategory
    }
    deriving (Show)

data RetTypeMismatch = RetTypeMismatch
    { retTypeMismatchRetType :: TypeCategory
    , retTypeMismatchExpType :: TypeCategory
    }
    deriving (Show)

data InvalidReturnError = InvalidReturnError
    { invalidReturnErrorFn :: Token
    }
    deriving (Show)

data DuplicateFnDefnError = DuplicateFnDefnError
    { duplicateFnDefnName1 :: Token
    , duplicateFnDefnName2 :: Token
    }
    deriving (Show)

data VerificationError
    = USE_BEFORE_DECL UseBeforeDeclarationError
    | DOUBLE_DECL DoubleDeclarationError
    | USE_BEFORE_ASN UseBeforeAssignmentError
    | OP_TYPE_MISMATCH OpTypeMismatch
    | ASN_TYPE_MISMATCH AsnTypeMismatch
    | IF_COND_TYPE_MISMATCH IfCondTypeMismatch
    | RET_TYPE_MISMATCH RetTypeMismatch
    | INVALID_RET InvalidReturnError
    | DUPLICATE_FN_DEFN DuplicateFnDefnError
    deriving (Show)
