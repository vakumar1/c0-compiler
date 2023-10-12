module Parser (
    parser,
) where

import Ast
import Errors
import Tokens

parser :: [Token] -> (Maybe Exp, [ParserError])
parser tokens = parserHelper [] tokens []

-- TMP: only parses "$" delimited expressions with intconst, identifiers, binop, unop, (, and )
parserHelper :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
parserHelper currNodes tokens errors =
    case (head tokens) of
        EOF -> case currNodes of
            [] ->
                (Nothing, [EmptyExpressionError])
            EXP_NODE e : [] ->
                (Just e, errors)
            TOKEN_NODE OPEN_PAREN : leftover ->
                parserHelper leftover tokens (errors ++ [DanglingOpenParen])
            TOKEN_NODE b : EXP_NODE e : leftover
                | isbinop b ->
                    parserHelper ((EXP_NODE e) : leftover) tokens (errors ++ [DanglingBinaryOp])
            TOKEN_NODE u : leftover
                | isunop u ->
                    parserHelper leftover tokens (errors ++ [DanglingOpenParen])
            _ -> case (tryReduce currNodes) of
                Just reducedNodes -> parserHelper reducedNodes tokens errors
                _ -> error (compilerError "entered unknown internal state when parsing EOF token")

-- Token lit | elem lit [IDENTIFIER, HEXNUM, DECNUM]
-- OPEN_PAREN
-- CLOSE_PAREN
-- Token strict_binop | elem strict_binop [PLUS, STAR, SLASH, PERC]
-- DASH

tryReduce :: [Node] -> Maybe [Node]
tryReduce currNodes =
    case currNodes of
        TOKEN_NODE CLOSE_PAREN : EXP_NODE e : TOKEN_NODE OPEN_PAREN : leftover ->
            Just ((EXP_NODE e) : leftover)
        EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : leftover
            | isbinop b ->
                Just ((EXP_NODE (BINOP_EXP (Binop b e1 e2))) : leftover)
        EXP_NODE e : TOKEN_NODE u : leftover
            | isunop u ->
                Just ((EXP_NODE (UNOP_EXP (Unop u e))) : leftover)
        _ -> Nothing

isbinop :: Token -> Bool
isbinop b = elem b [PLUS, DASH, STAR, SLASH, PERC]

isunop :: Token -> Bool
isunop u = u == DASH
