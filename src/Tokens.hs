module Tokens (
    Token
) where

newtype Token = Token String
getAssociatedStr :: Token -> String
getAssociatedStr (Token s) = s
