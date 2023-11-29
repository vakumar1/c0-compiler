module Types (

) where

import Tokens

data TypeCategory
    = INT_TYPE

data Type = Type
    { typeCategory :: TypeCategory
    , typeToken :: Token
    }
