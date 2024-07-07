module Common.Errors (
    LexerErrorCategory (..),
    LexerError (..),
    UseBeforeDeclarationError (..),
    DoubleDeclarationError (..),
    UseBeforeAssignmentError (..),
    OpTypeMismatch (..),
    RefTypeMismatch (..),
    AsnTypeMismatch (..),
    AsnTypeDereference (..),
    AsnTypeIndex (..),
    ArrIndexNonIntType (..),
    IfCondTypeMismatch (..),
    RetTypeMismatch (..),
    InvalidReturnError (..),
    ConflictingFnDeclError (..),
    DuplicateFnDefnError (..),
    VerificationError (..),
    ArgMismatchError (..),
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

data RefTypeMismatch = RefTypeMismatch
    { refTypeMismatchRefTok :: Token
    }
    deriving (Show)

data AsnTypeMismatch = AsnTypeMismatch
    { asnTypeMismatchVar :: Token
    , asnTypeMismatchAsnType :: TypeCategory
    , asnTypeMismatchExpType :: TypeCategory
    }
    deriving (Show)

data AsnTypeDereference = AsnTypeDereference
    { asnTypeDereferenceVar :: Token
    , asnTypeDereferenceVarType :: TypeCategory
    }
    deriving (Show)

data AsnTypeIndex = AsnTypeIndex
    { asnTypeIndexVar :: Token
    , asnTypeIndexVarType :: TypeCategory
    }
    deriving (Show)

data ArrIndexNonIntType = ArrIndexNonIntType
    { arrIndexNonIntTypeVar :: Token
    , arrIndexNonIntType :: TypeCategory
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

data ConflictingFnDeclError = ConflictingFnDeclError
    { conflictFnDecl1 :: Token
    , conflictingFnDecl1Args :: [TypeCategory]
    , conflictingFnDecl1Ret :: TypeCategory
    , conflictFnDecl2 :: Token
    , conflictingFnDecl2Args :: [TypeCategory]
    , conflictingFnDecl2Ret :: TypeCategory
    }
    deriving (Show)

data DuplicateFnDefnError = DuplicateFnDefnError
    { duplicateFnDefnName1 :: Token
    , duplicateFnDefnName2 :: Token
    }
    deriving (Show)

data ArgMismatchError = ArgMismatchError
    { argMistmatchFnCaller :: Token 
    , argMismatchFnArgs :: [TypeCategory]
    , argMismatchCallerArgs :: [TypeCategory]
    }
    deriving (Show)

data VerificationError
    = USE_BEFORE_DECL UseBeforeDeclarationError
    | DOUBLE_DECL DoubleDeclarationError
    | USE_BEFORE_ASN UseBeforeAssignmentError
    | OP_TYPE_MISMATCH OpTypeMismatch
    | REF_TYPE_MISMATCH RefTypeMismatch
    | ASN_TYPE_MISMATCH AsnTypeMismatch
    | ASN_TYPE_DEREF AsnTypeDereference
    | ASN_TYPE_INDEX AsnTypeIndex
    | ARR_INDEX_NON_INT_TYPE ArrIndexNonIntType
    | IF_COND_TYPE_MISMATCH IfCondTypeMismatch
    | RET_TYPE_MISMATCH RetTypeMismatch
    | INVALID_RET InvalidReturnError
    | CONFLICTING_FN_DECL ConflictingFnDeclError
    | DUPLICATE_FN_DEFN DuplicateFnDefnError
    | ARG_MISMATCH ArgMismatchError
    deriving (Show)
