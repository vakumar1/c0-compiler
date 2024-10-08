module Common.Errors (
    DuplicateTypedefDefnError (..),
    DuplicateStructDefnError (..),
    UndeclaredStructError (..),
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
    AsnTypeFieldAccess (..),
    ArrIndexNonIntType (..),
    StructUndefined (..),
    StructFieldUndefined (..),
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

data DuplicateTypedefDefnError = DuplicateTypedefDefnError
    { duplicateTypedefDefnAlias :: Token
    , duplicateTypedefDefnTarget1 :: TypeCategory
    , duplicateTypedefDefnTarget2 :: TypeCategory
    }
    deriving (Show)

data DuplicateStructDefnError = DuplicateStructDefnError
    { duplicateStructDefnTarget1 :: TypeCategory
    , duplicateStructDefnTarget2 :: TypeCategory
    }
    deriving (Show)

data UndeclaredStructError = UndeclaredStructError
    { undeclaredStructTok :: Token
    }
    deriving (Show)

data LexerErrorCategory
    = INVALID_TOKEN
    | DANGLING_COMMENT
    | DANGLING_OPEN_ENCLOSER
    | DANGLING_CLOSED_ENCLOSER
    | NONEXISTENT_TYPE NonexistentTypeError
    | CIRCULAR_TYPE_RESOLUTION CircularTypeResolutionError
    | DUPLICATE_TYPEDEF_DEFN DuplicateTypedefDefnError
    | DUPLICATE_STRUCT_DEFN DuplicateStructDefnError
    | UNDECLARED_STRUCT UndeclaredStructError
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

data AsnTypeFieldAccess = AsnTypeFieldAccess
    { asnTypeFieldAccessVar :: Token
    , asnTypeFieldAccessVarType :: TypeCategory
    }
    deriving (Show)

data ArrIndexNonIntType = ArrIndexNonIntType
    { arrIndexNonIntTypeVar :: Token
    , arrIndexNonIntType :: TypeCategory
    }
    deriving (Show)

data StructUndefined = StructUndefined
    { structUndefinedStructTok :: Token
    }
    deriving (Show)

data StructFieldUndefined = StructFieldUndefined
    { structFieldUndefinedStructTok :: Token
    , structFieldUndefinedFieldTok :: Token
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
    | ASN_TYPE_FIELD_ACCESS AsnTypeFieldAccess
    | ARR_INDEX_NON_INT_TYPE ArrIndexNonIntType
    | STRUCT_UNDEFINED StructUndefined
    | STRUCT_FIELD_UNDEFINED StructFieldUndefined
    | IF_COND_TYPE_MISMATCH IfCondTypeMismatch
    | RET_TYPE_MISMATCH RetTypeMismatch
    | INVALID_RET InvalidReturnError
    | CONFLICTING_FN_DECL ConflictingFnDeclError
    | DUPLICATE_FN_DEFN DuplicateFnDefnError
    | ARG_MISMATCH ArgMismatchError
    deriving (Show)
