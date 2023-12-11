module Types (
    TypeCategory (..),
    Const (..),
) where

import Tokens

data TypeCategory
    = INT_TYPE
    deriving (Show)

data Const
    = INT_CONST Int
    deriving (Show)
