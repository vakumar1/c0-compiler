module Parser (
    parser,
) where

import Ast
import Errors
import Tokens

parser :: [Token] -> (Maybe Statements, [ParserError])
parser tokens = stmtsHelper [] tokens []

-- STMTS HANDLERS

stmtsHelper :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtsHelper currNodes tokens errors =
    case (tokenCat (head tokens)) of
        EOF -> stmtsHandleEOF currNodes tokens errors
        _ -> stmtHelper currNodes tokens errors

stmtsHandleEOF :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtsHandleEOF currNodes tokens errors =
    let reduce = stmtsHelper (stmtsTryReduce currNodes (head tokens)) tokens errors
     in case currNodes of
            l | stmtsHitBarrier l -> (Nothing, errors)
            STMTS_NODE ss : l | stmtsHitBarrier l -> (Just ss, errors)
            _ -> reduce

-- STMTS HELPERS

stmtsHitBarrier :: [Node] -> Bool
stmtsHitBarrier currNodes =
    case currNodes of
        [] -> True
        _ -> False

stmtsTryReduce :: [Node] -> Token -> [Node]
stmtsTryReduce currNodes lookahead =
    case currNodes of
        STMT_NODE s : _ -> stmtsSmushLeadingStmt [] currNodes
        _ -> error (compilerError ("entered unknown internal state when parsing stmts. currNodes=" ++ (show currNodes)))

stmtsSmushLeadingStmt :: Statements -> [Node] -> [Node]
stmtsSmushLeadingStmt acc currNodes =
    case currNodes of
        STMT_NODE s : _ -> stmtsSmushLeadingStmt (s : acc) (tail currNodes)
        _ -> (STMTS_NODE (acc)) : currNodes

-- STMT HANDLERS

stmtHelper :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHelper currNodes tokens errors =
    case (tokenCat (head tokens)) of
        EOF -> stmtHandleEOF currNodes tokens errors
        SEMICOLON -> stmtHandleSemicolon currNodes tokens errors
        RETURN -> stmtHandleReturn currNodes tokens errors
        INT -> stmtHandleInt currNodes tokens errors
        IDENTIFIER _ -> stmtHandleIdentifier currNodes tokens errors
        EQUAL -> stmtHandleEqual currNodes tokens errors
        _ -> stmtHandleUnexpected currNodes tokens errors

stmtHandleEOF :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHandleEOF currNodes tokens errors =
    let discardDangler = \prevNodes err -> expHelper prevNodes tokens (errors ++ [err])
        reduce = stmtHelper (stmtTryReduce currNodes (head tokens)) tokens errors
     in case currNodes of
            l | stmtHitBarrier l -> stmtsHelper currNodes tokens (errors ++ [ParserError ExpectedSemicolon (head tokens)])
            STMT_NODE s : l | stmtHitBarrier l -> stmtsHelper currNodes tokens (errors ++ [ParserError ExpectedSemicolon (head tokens)])
            TOKEN_NODE t : _ | istype (tokenCat t) -> discardDangler (tail currNodes) (ParserError ExpectedSemicolon (head tokens))
            _ -> reduce

stmtHandleSemicolon :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHandleSemicolon currNodes tokens errors =
    let discardDangler = \prevNodes err -> expHelper prevNodes tokens (errors ++ [err])
        reduce = stmtHelper (stmtTryReduce currNodes (head tokens)) tokens errors
     in case currNodes of
            l | stmtHitBarrier l -> stmtsHelper currNodes (tail tokens) errors
            STMT_NODE s : l | stmtHitBarrier l -> stmtsHelper currNodes (tail tokens) errors
            TOKEN_NODE t : _ | istype (tokenCat t) -> discardDangler (tail currNodes) (ParserError ExpectedSemicolon (head tokens))
            _ -> reduce

stmtHandleUnexpected :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHandleUnexpected currNodes tokens errors = stmtHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedTokenInStatement (head tokens)])

stmtHandleReturn :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHandleReturn currNodes tokens errors =
    let discardLookahead = stmtHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedReturn (head tokens)])
        reduce = stmtHelper (stmtTryReduce currNodes (head tokens)) tokens errors
        shiftLookahead = expHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
     in case currNodes of
            l | stmtHitBarrier l -> shiftLookahead
            TOKEN_NODE t : _ | istype (tokenCat t) -> discardLookahead
            TOKEN_NODE a : TOKEN_NODE t : _ | isident (tokenCat a) && istype (tokenCat t) -> discardLookahead
            _ -> reduce

stmtHandleInt :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHandleInt currNodes tokens errors =
    let discardLookahead = stmtHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedType (head tokens)])
        reduce = stmtHelper (stmtTryReduce currNodes (head tokens)) tokens errors
        shiftLookahead = stmtHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
     in case currNodes of
            l | stmtHitBarrier l -> shiftLookahead
            TOKEN_NODE t : _ | istype (tokenCat t) -> discardLookahead
            TOKEN_NODE a : TOKEN_NODE t : _ | isident (tokenCat a) && istype (tokenCat t) -> discardLookahead
            _ -> reduce

stmtHandleIdentifier :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHandleIdentifier currNodes tokens errors =
    let discardLookahead = stmtHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedIdentifier (head tokens)])
        reduce = stmtHelper (stmtTryReduce currNodes (head tokens)) tokens errors
        shiftLookahead = stmtHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
     in case currNodes of
            l | stmtHitBarrier l -> discardLookahead
            TOKEN_NODE t : _ | istype (tokenCat t) -> shiftLookahead
            TOKEN_NODE a : TOKEN_NODE t : _ | isident (tokenCat a) && istype (tokenCat t) -> discardLookahead
            _ -> reduce

stmtHandleEqual :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
stmtHandleEqual currNodes tokens errors =
    let discardLookahead = stmtHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedASNOp (head tokens)])
        reduce = stmtHelper (stmtTryReduce currNodes (head tokens)) tokens errors
        shiftLookahead = expHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
     in case currNodes of
            l | stmtHitBarrier l -> discardLookahead
            TOKEN_NODE t : _ | istype (tokenCat t) -> discardLookahead
            TOKEN_NODE a : TOKEN_NODE t : _ | isident (tokenCat a) && istype (tokenCat t) -> shiftLookahead
            _ -> reduce

-- STMT HELPERS

stmtHitBarrier :: [Node] -> Bool
stmtHitBarrier currNodes =
    case currNodes of
        [] -> True
        STMTS_NODE _ : _ -> True
        STMT_NODE _ : _ -> True
        _ -> False

stmtTryReduce :: [Node] -> Token -> [Node]
stmtTryReduce currNodes lookahead =
    case currNodes of
        TOKEN_NODE a : TOKEN_NODE t : _
            | istype (tokenCat t) && isident (tokenCat a) ->
                (STMT_NODE (STMT_DECL (Decl a Nothing))) : (drop 2 currNodes)
        EXP_NODE e : TOKEN_NODE (Token EQUAL _) : TOKEN_NODE a : TOKEN_NODE t : _
            | istype (tokenCat t) && isident (tokenCat a) ->
                (STMT_NODE (STMT_DECL (Decl a (Just e)))) : (drop 4 currNodes)
        EXP_NODE e : TOKEN_NODE (Token RETURN _) : _ -> STMT_NODE (STMT_RET e) : (drop 2 currNodes)
        _ -> error (compilerError ("entered unknown internal state when parsing stmt. token=" ++ (show lookahead)))

istype :: TokenCategory -> Bool
istype t = t == INT

isident :: TokenCategory -> Bool
isident t =
    case t of
        IDENTIFIER _ -> True
        _ -> False

-- EXP HANDLERS

expHelper :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHelper currNodes tokens errors =
    case (tokenCat (head tokens)) of
        EOF -> expHandleTerminator currNodes tokens errors
        SEMICOLON -> expHandleTerminator currNodes tokens errors
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
        _ -> expHandleUnexpected currNodes tokens errors

expHandleTerminator :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHandleTerminator currNodes tokens errors =
    let discardDangler = \prevNodes err -> expHelper prevNodes tokens (errors ++ [err])
        reduce = expHelper (expTryReduce currNodes (head tokens)) tokens errors
     in case currNodes of
            l | expHitBarrier l -> stmtHelper currNodes tokens (errors ++ [ParserError EmptyExpression (head tokens)])
            EXP_NODE e : l | expHitBarrier l -> stmtHelper currNodes tokens errors
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

expHandleUnexpected :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHandleUnexpected currNodes tokens errors = expHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedTokenInExpression (head tokens)])

expHandleAtomic :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHandleAtomic currNodes tokens errors =
    let discardLookahead = expHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedExpression (head tokens)])
        shiftLookahead = expHelper ((EXP_NODE (IDENTIFIER_EXP (head tokens))) : currNodes) (tail tokens) (errors)
     in case currNodes of
            TOKEN_NODE (Token CLOSE_PAREN _) : EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            EXP_NODE e : _ -> discardLookahead
            TOKEN_NODE t : _ | isatomic (tokenCat t) -> discardLookahead
            _ -> shiftLookahead

expHandleDash :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHandleDash currNodes tokens errors =
    let shiftLookahead = expHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
        reduce = expHelper (expTryReduce currNodes (head tokens)) tokens errors
     in case currNodes of
            l | expHitBarrier l -> shiftLookahead
            EXP_NODE e : l | expHitBarrier l -> shiftLookahead
            TOKEN_NODE (Token OPEN_PAREN _) : _ -> shiftLookahead
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> shiftLookahead
            EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : _ | isbinop (tokenCat b) && strictGreaterPrecedence DASH (tokenCat b) -> shiftLookahead
            TOKEN_NODE b : EXP_NODE e : _ | isbinop (tokenCat b) -> shiftLookahead
            TOKEN_NODE u : _ | isunop (tokenCat u) -> shiftLookahead
            _ -> reduce

expHandleStrictBinop :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHandleStrictBinop currNodes tokens errors =
    let discardLookahead = expHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedExpression (head tokens)])
        shiftLookahead = expHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
        reduce = expHelper (expTryReduce currNodes (head tokens)) tokens errors
     in case currNodes of
            l | expHitBarrier l -> discardLookahead
            TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            TOKEN_NODE b : EXP_NODE e : _ | isbinop (tokenCat b) -> discardLookahead
            TOKEN_NODE u : _ | isunop (tokenCat u) -> discardLookahead
            EXP_NODE e : l | expHitBarrier l -> shiftLookahead
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> shiftLookahead
            EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : _
                | isbinop (tokenCat b) && strictGreaterPrecedence (tokenCat (head tokens)) (tokenCat b) ->
                    shiftLookahead
            _ -> reduce

expHandleOpenParen :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHandleOpenParen currNodes tokens errors =
    let discardLookahead = expHelper currNodes (tail tokens) (errors ++ [ParserError UnexpectedExpression (head tokens)])
        shiftLookahead = expHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
     in case currNodes of
            EXP_NODE e : l | expHitBarrier l -> discardLookahead
            EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            TOKEN_NODE (Token CLOSE_PAREN _) : EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : _ -> discardLookahead
            EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : _ | isbinop (tokenCat b) -> discardLookahead
            EXP_NODE e : TOKEN_NODE u : _ | isunop (tokenCat u) -> discardLookahead
            _ -> shiftLookahead

expHandleCloseParen :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expHandleCloseParen currNodes tokens errors =
    let discardDangler = \prevNodes err -> expHelper prevNodes tokens (errors ++ [err])
        discardLookahead = expHelper currNodes (tail tokens) (errors ++ [ParserError DanglingCloseParen (head tokens)])
        shiftLookahead = expHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors
        reduce = expHelper (expTryReduce currNodes (head tokens)) tokens errors
     in case currNodes of
            l | expHitBarrier l -> discardLookahead
            EXP_NODE e : l | expHitBarrier l -> discardLookahead
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

expReduce :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expReduce currNodes tokens errors = expHelper (expTryReduce currNodes (head tokens)) tokens errors

expShiftLookahead :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expShiftLookahead currNodes tokens errors = expHelper ((TOKEN_NODE (head tokens)) : currNodes) (tail tokens) errors

expDiscardLookahead :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expDiscardLookahead currNodes tokens newErrors = expHelper currNodes (tail tokens) newErrors

expDiscardDangler :: [Node] -> [Token] -> [ParserError] -> (Maybe Statements, [ParserError])
expDiscardDangler prevNodes tokens newErrors = expHelper prevNodes tokens newErrors

-- EXP HELPERS

expHitBarrier :: [Node] -> Bool
expHitBarrier nodes =
    case nodes of
        [] -> True
        STMT_NODE s : _ -> True
        STMTS_NODE ss : _ -> True
        TOKEN_NODE (Token RETURN _) : _ -> True
        TOKEN_NODE (Token EQUAL _) : _ -> True
        _ -> False

expTryReduce :: [Node] -> Token -> [Node]
expTryReduce currNodes lookahead =
    case currNodes of
        TOKEN_NODE (Token CLOSE_PAREN _) : EXP_NODE e : TOKEN_NODE (Token OPEN_PAREN _) : leftover ->
            (EXP_NODE e) : leftover
        EXP_NODE e2 : TOKEN_NODE b : EXP_NODE e1 : leftover
            | isbinop (tokenCat b) ->
                (EXP_NODE (BINOP_EXP (Binop b e1 e2))) : leftover
        EXP_NODE e : TOKEN_NODE u : leftover
            | isunop (tokenCat u) ->
                (EXP_NODE (UNOP_EXP (Unop u e))) : leftover
        _ -> error (compilerError ("entered unknown internal state when parsing exp. token=" ++ (show lookahead)))

isbinop :: TokenCategory -> Bool
isbinop b = elem b [PLUS, DASH, STAR, SLASH, PERC]

isunop :: TokenCategory -> Bool
isunop u = u == DASH

isatomic :: TokenCategory -> Bool
isatomic at =
    case at of
        IDENTIFIER a -> True
        HEXNUM a -> True
        DECNUM a -> True
        _ -> False

strictGreaterPrecedence :: TokenCategory -> TokenCategory -> Bool
strictGreaterPrecedence rightBinop leftBinop =
    if elem rightBinop [PLUS, DASH]
        then False
        else
            if elem rightBinop [STAR, SLASH, PERC]
                then elem leftBinop [PLUS, DASH]
                else error (compilerError "unrecognized binop token")
