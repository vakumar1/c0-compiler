module Model.Types (
    TypeCategory (..),
    Const (..),
    constToType,
) where

import Model.Tokens

data TypeCategory
    = INT_TYPE
    | BOOL_TYPE
    | VOID_TYPE
    deriving (Eq, Show)

data Const
    = INT_CONST Int
    | BOOL_CONST Bool
    deriving (Show)

constToType :: Const -> TypeCategory
constToType const =
    case const of
        INT_CONST _ -> INT_TYPE
        BOOL_CONST _ -> BOOL_TYPE
