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
