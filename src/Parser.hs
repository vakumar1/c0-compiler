module Parser (
    parser,
) where

import Ast
import Errors
import Tokens

parser :: [Token] -> (Maybe Exp, [ParserError])
parser tokens = parserHelper [] tokens []

parserHelper :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
parserHelper currNodes tokens errors =
    case (tokenCat (head tokens)) of
        EOF -> expHandleEOF currNodes tokens errors
        IDENTIFIER _ -> expHandleAtomic currNodes tokens errors
        HEXNUM _ -> expHandleAtomic currNodes tokens errors
        DECNUM _ -> expHandleAtomic currNodes tokens errors
        DASH -> expHandleDash currNodes tokens errors
        PLUS -> expHandleStrictBinop currNodes tokens errors
        STAR -> expHandleStrictBinop currNodes tokens errors
        SLASH -> expHandleStrictBinop currNodes tokens errors
        PERC -> expHandleStrictBinop currNodes tokens errors
        OPEN_PAREN -> expHandleOpenParen currNodes tokens errors
        CLOSE_PAREN -> expHandleCloseParen currNodes tokens errors

expHandleEOF :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
expHandleEOF currNodes tokens errors =
    let discardDangler = \prevNodes err -> parserHelper prevNodes tokens (errors ++ [err])
        reduce = case (tryReduce currNodes) of
            Just reducedNodes -> parserHelper reducedNodes tokens errors
            _ -> error (compilerError "entered unknown internal state when parsing EOF token" ++ (show currNodes))
     in case currNodes of
            [] -> (Nothing, errors ++ [ParserError EmptyExpression (head tokens)])
            EXP_NODE e : [] -> (Just e, errors)
            TOKEN_NODE (Token OPEN_PAREN d) : _ ->
                discardDangler (tail currNodes) (ParserError DanglingOpenParen (Token OPEN_PAREN d))
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN d) : leftover ->
                discardDangler ((head currNodes) : leftover) (ParserError DanglingOpenParen (Token OPEN_PAREN d))
            TOKEN_NODE b : EXP_NODE e : _
                | isbinop (tokenCat b) ->
                    discardDangler (tail currNodes) (ParserError DanglingBinaryOp b)
            TOKEN_NODE u : _
                | isunop (tokenCat u) ->
                    discardDangler (tail currNodes) (ParserError DanglingUnaryOp u)
            _ -> reduce

expHandleAtomic :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
expHandleAtomic currNodes tokens errors =
    let discardLookahead = parserHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedExpression (head tokens)])
        shiftLookahead = parserHelper ((EXP_NODE (IDENTIFIER_EXP (head tokens))) : currNodes) (tail tokens) (errors)
     in case currNodes of
            TOKEN_NODE (Token CLOSE_PAREN _) : EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            EXP_NODE e : _ -> discardLookahead
            TOKEN_NODE t : _ | isatomic (tokenCat t) -> discardLookahead
            _ -> shiftLookahead

expHandleDash :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
expHandleDash currNodes tokens errors =
    let shiftLookahead = parserHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
        reduce = case (tryReduce currNodes) of
            Just reducedNodes -> parserHelper reducedNodes tokens errors
            _ -> error (compilerError "entered unknown internal state when parsing DASH token")
     in case currNodes of
            [] -> shiftLookahead
            EXP_NODE e : [] -> shiftLookahead
            TOKEN_NODE (Token OPEN_PAREN _) : _ -> shiftLookahead
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> shiftLookahead
            EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : _ | isbinop (tokenCat b) && strictGreaterPrecedence DASH (tokenCat b) -> shiftLookahead
            TOKEN_NODE b : EXP_NODE e : _ | isbinop (tokenCat b) -> shiftLookahead
            TOKEN_NODE u : _ | isunop (tokenCat u) -> shiftLookahead
            _ -> reduce

expHandleStrictBinop :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
expHandleStrictBinop currNodes tokens errors =
    let discardLookahead = parserHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedExpression (head tokens)])
        shiftLookahead = parserHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
        reduce = case (tryReduce currNodes) of
            Just reducedNodes -> parserHelper reducedNodes tokens errors
            _ -> error (compilerError "entered unknown internal state when parsing BINOP token")
     in case currNodes of
            [] -> discardLookahead
            TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            TOKEN_NODE b : EXP_NODE e : _ | isbinop (tokenCat b) -> discardLookahead
            TOKEN_NODE u : _ | isunop (tokenCat u) -> discardLookahead
            EXP_NODE e : [] -> shiftLookahead
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> shiftLookahead
            EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : _
                | isbinop (tokenCat b) && strictGreaterPrecedence (tokenCat (head tokens)) (tokenCat b) ->
                    shiftLookahead
            _ -> reduce

expHandleOpenParen :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
expHandleOpenParen currNodes tokens errors =
    let discardLookahead = parserHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedExpression (head tokens)])
        shiftLookahead = parserHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
     in case currNodes of
            EXP_NODE e : [] -> discardLookahead
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            TOKEN_NODE (Token CLOSE_PAREN _) : EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : _ | isbinop (tokenCat b) -> discardLookahead
            EXP_NODE e : TOKEN_NODE u : _ | isunop (tokenCat u) -> discardLookahead
            _ -> shiftLookahead

expHandleCloseParen :: [Node] -> [Token] -> [ParserError] -> (Maybe Exp, [ParserError])
expHandleCloseParen currNodes tokens errors =
    let discardDangler = \prevNodes err -> parserHelper prevNodes tokens (errors ++ [err])
        discardLookahead = parserHelper currNodes (tail tokens) (errors ++ [ParserError DanglingCloseParen (head tokens)])
        shiftLookahead = parserHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
        reduce = case (tryReduce currNodes) of
            Just reducedNodes -> parserHelper reducedNodes tokens errors
            _ -> error (compilerError "entered unknown internal state when parsing (Token CLOSE_PAREN _) token")
     in case currNodes of
            [] -> discardLookahead
            EXP_NODE e : [] -> discardLookahead
            TOKEN_NODE (Token OPEN_PAREN _) : _ ->
                discardDangler (tail currNodes) (ParserError ExpectedExpression (head tokens)) -- TODO: set a policy for ()
            TOKEN_NODE b : EXP_NODE e : _
                | isbinop (tokenCat b) ->
                    discardDangler (tail currNodes) (ParserError DanglingBinaryOp b)
            TOKEN_NODE u : _
                | isunop (tokenCat u) ->
                    discardDangler (tail currNodes) (ParserError DanglingUnaryOp u)
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> shiftLookahead
            _ -> reduce

tryReduce :: [Node] -> Maybe [Node]
tryReduce currNodes =
    case currNodes of
        TOKEN_NODE (Token CLOSE_PAREN _) : EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : leftover ->
            Just ((EXP_NODE e) : leftover)
        EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : leftover
            | isbinop (tokenCat b) ->
                Just ((EXP_NODE (BINOP_EXP (Binop b e1 e2))) : leftover)
        EXP_NODE e : TOKEN_NODE u : leftover
            | isunop (tokenCat u) ->
                Just ((EXP_NODE (UNOP_EXP (Unop u e))) : leftover)
        _ -> Nothing

isbinop :: TokenCategory -> Bool
isbinop b = elem b [PLUS, DASH, STAR, SLASH, PERC]

isunop :: TokenCategory -> Bool
isunop u = u == DASH

isatomic :: TokenCategory -> Bool
isatomic a =
    case a of
        IDENTIFIER s -> True
        HEXNUM s -> True
        DECNUM s -> True
        _ -> False

strictGreaterPrecedence :: TokenCategory -> TokenCategory -> Bool
strictGreaterPrecedence rightBinop leftBinop =
    if elem rightBinop [PLUS, DASH]
        then False
        else
            if elem rightBinop [STAR, SLASH, PERC]
                then elem leftBinop [PLUS, DASH]
                else error (compilerError "unrecognized binop token")
