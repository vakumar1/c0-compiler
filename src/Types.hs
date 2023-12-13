module Types (
    TypeCategory (..),
    Const (..),
    constToType,
) where

import Tokens

data TypeCategory
    = INT_TYPE
    deriving (Show)

data Const
    = INT_CONST Int
    deriving (Show)

constToType :: Const -> TypeCategory
constToType const = 
    case const of
        INT_CONST _ -> INT_TYPE
