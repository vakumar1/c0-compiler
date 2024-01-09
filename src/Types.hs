module Types (
    TypeCategory (..),
    Const (..),
    constToType,
) where

import Tokens

data TypeCategory
    = INT_TYPE
    | BOOL_TYPE
    deriving (Eq, Show)

data Const
    = INT_CONST Int
    | BOOL_TRUE_CONST
    | BOOL_FALSE_CONST
    deriving (Show)

constToType :: Const -> TypeCategory
constToType const =
    case const of
        INT_CONST _ -> INT_TYPE
        BOOL_TRUE_CONST -> BOOL_TYPE
        BOOL_FALSE_CONST -> BOOL_TYPE
