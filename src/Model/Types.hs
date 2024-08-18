module Model.Types (
    TypeAliasContext (..),
    StructContext (..),
    emptyStructCtx,
    TypeCategory (..),
    Const (..),
    constToType,
    sizeofType,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type TypeAliasContext = Map.Map String TypeCategory

data StructContext = StructContext
    { structContextDefined :: Map.Map String [(String, TypeCategory)]
    , structContextDeclared :: Set.Set String
    }
    deriving Show
emptyStructCtx :: StructContext
emptyStructCtx = StructContext Map.empty Set.empty

data TypeCategory
    = INT_TYPE
    | BOOL_TYPE
    | CHAR_TYPE
    | STRING_TYPE
    | VOID_TYPE
    | POINTER_TYPE TypeCategory
    | ARRAY_TYPE TypeCategory Int
    | STRUCT_TYPE String
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

sizeofType :: StructContext -> TypeCategory -> Int
sizeofType structCtx ty = 
    case ty of
        INT_TYPE -> 8
        BOOL_TYPE -> 8
        POINTER_TYPE _ -> 8
        ARRAY_TYPE innerTy size -> (sizeofType structCtx innerTy) * size
        STRUCT_TYPE structName -> 
            case Map.lookup structName (structContextDefined structCtx) of
                Just structFields -> sum (map (\(_, ty) -> sizeofType structCtx ty) structFields)
                Nothing -> error ("No definition/declaration for struct: " ++ structName)
