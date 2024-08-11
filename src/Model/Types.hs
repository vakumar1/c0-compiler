module Model.Types (
    TypeAliasContext (..),
    emptyTypeAliasCtx,
    TypeCategory (..),
    Const (..),
    constToType,
    sizeofType,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

data TypeAliasContext = TypeAliasContext
    { typeAliasContextAliases :: Map.Map String TypeCategory
    , typeAliasContextStructs :: Map.Map String TypeCategory
    , typeAliasContextStructDecls :: Set.Set String
    }
emptyTypeAliasCtx :: TypeAliasContext
emptyTypeAliasCtx = TypeAliasContext Map.empty Map.empty Set.empty

data TypeCategory
    = INT_TYPE
    | BOOL_TYPE
    | CHAR_TYPE
    | STRING_TYPE
    | VOID_TYPE
    | POINTER_TYPE TypeCategory
    | ARRAY_TYPE TypeCategory Int
    | STRUCT_TYPE String [(String, TypeCategory)]
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
            (ARRAY_TYPE t1 s1, ARRAY_TYPE t2 s2) -> t1 == t2 && s1 == s2
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

sizeofType :: TypeCategory -> Int
sizeofType ty = 
    case ty of
        INT_TYPE -> 8
        BOOL_TYPE -> 8
        POINTER_TYPE _ -> 8
        ARRAY_TYPE innerTy size -> (sizeofType innerTy) * size
        STRUCT_TYPE _ innerITs -> sum (map (\(_, ty) -> sizeofType ty) innerITs)
