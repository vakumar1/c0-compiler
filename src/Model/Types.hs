module Model.Types (
    TypeAliasContext (..),
    TypeCategory (..),
    Const (..),
    constToType,
) where

import qualified Data.Map as Map

type TypeAliasContext = Map.Map String TypeCategory

data TypeCategory
    = INT_TYPE
    | BOOL_TYPE
    | CHAR_TYPE
    | STRING_TYPE
    | VOID_TYPE
    | POINTER_TYPE TypeCategory
    | WILDCARD_TYPE
    deriving (Show)
instance Eq TypeCategory where
    ty1 == ty2 = 
        case (ty1, ty2) of
            (WILDCARD_TYPE, _) -> True
            (_, WILDCARD_TYPE) -> True
            (INT_TYPE, INT_TYPE) -> True
            (BOOL_TYPE, BOOL_TYPE) -> True
            (CHAR_TYPE, CHAR_TYPE) -> True
            (STRING_TYPE, STRING_TYPE) -> True
            (VOID_TYPE, VOID_TYPE) -> True
            (POINTER_TYPE s1, POINTER_TYPE s2) -> s1 == s2
            _ -> False

data Const
    = INT_CONST Int
    | BOOL_CONST Bool
    | NULL_CONST
    deriving (Show)

constToType :: Const -> TypeCategory
constToType const =
    case const of
        INT_CONST _ -> INT_TYPE
        BOOL_CONST _ -> BOOL_TYPE
        NULL_CONST -> POINTER_TYPE WILDCARD_TYPE
