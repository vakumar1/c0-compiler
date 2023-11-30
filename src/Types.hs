module Types (
    TypeCategory (..),
    Type (..),
) where

import Tokens

data TypeCategory
    = INT_TYPE
    deriving (Show)

data Type = Type
    { typeCategory :: TypeCategory
    , typeToken :: Token
    }
    deriving (Show)
