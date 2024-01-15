module Lexer (
    lexer,
) where

import Errors
import Tokens

import qualified Data.Maybe as Maybe
import qualified Data.Char as C
import qualified Data.Foldable as F

lineNoStart :: Int
lineNoStart = 1
linePosStart :: Int
linePosStart = 1

lexer :: String -> ([Token], [LexerError])
lexer code = lexerHelper [] [] [] "" (code ++ ['\n']) lineNoStart linePosStart

lexerHelper :: [Token] -> [Token] -> [LexerError] -> String -> String -> Int -> Int -> ([Token], [LexerError])
lexerHelper finishedTokens openerStack errors currToken remainingStr lineNo linePos
    -- EOF
    | null remainingStr =
        let errorsAddDangler = foldr (\o l -> (LexerError ((tokenLineNo . tokenData) o) ((tokenLinePos . tokenData) o) DanglingOpenEncloserError) : l) errors openerStack
         in (finishedTokens, errorsAddDangler)
    -- delimiters
    | isDelim remainingStr =
        -- try to process current token and delimiter
        let (delimTokenCat, delimLength) = case classifyDelim remainingStr of
                Just (c, l) -> (Just c, l)
                Nothing -> (Nothing, 1)
            newRemainingStr = (drop delimLength remainingStr)
            finishedTokenCat = classifyToken currToken
            tokensAddCurr =
                if currToken == ""
                    then finishedTokens
                    else case finishedTokenCat of
                        Just t -> (Token t (TokenData lineNo (linePos - (length currToken)))) : finishedTokens
                        _ -> finishedTokens
            newFinishedTokens =
                case delimTokenCat of
                    Just c -> (Token c (TokenData lineNo linePos)) : tokensAddCurr
                    Nothing -> tokensAddCurr
            newErrors =
                if currToken == ""
                    then errors
                    else case finishedTokenCat of
                        Nothing -> ((LexerError lineNo (linePos - (length currToken)) InvalidTokenError)) : errors
                        _ -> errors
         in case remainingStr of
                -- comment: immediately go into comment mode
                '/' : '*' : _ ->
                    multilineComment tokensAddCurr openerStack newErrors (drop 2 remainingStr) lineNo linePos lineNo linePos
                '/' : '/' : _ ->
                    oneLineComment tokensAddCurr openerStack newErrors (drop 2 remainingStr) lineNo
                -- whitespace
                '\n' : _ ->
                    lexerHelper tokensAddCurr openerStack newErrors "" newRemainingStr (lineNo + 1) 0
                _
                    | elem (head remainingStr) whitespaceDelims ->
                        lexerHelper tokensAddCurr openerStack newErrors "" newRemainingStr lineNo (linePos + 1)
                -- open enclosure: add delim token to tokens and add opener to stack
                _
                    | elem (head remainingStr) "{[(" ->
                        lexerHelper newFinishedTokens ((head newFinishedTokens) : openerStack) newErrors "" newRemainingStr lineNo (linePos + 1)
                -- close enclousre: validate against stack and try to collapse the enclosure
                _
                    | elem (head remainingStr) "}])" ->
                        if closerMatchesLastOpener openerStack (head remainingStr)
                            then lexerHelper newFinishedTokens (tail openerStack) newErrors "" newRemainingStr lineNo (linePos + 1)
                            else
                                let errorsAddDangler = ((LexerError lineNo linePos DanglingClosedEncloserError) : newErrors)
                                 in lexerHelper (tail newFinishedTokens) openerStack errorsAddDangler "" newRemainingStr lineNo (linePos + 1)
                -- general delim case: add delim token to tokens
                _ ->
                    lexerHelper newFinishedTokens openerStack newErrors "" newRemainingStr lineNo (linePos + delimLength)
    -- general case
    | otherwise = lexerHelper finishedTokens openerStack errors (currToken ++ [(head remainingStr)]) (tail remainingStr) lineNo (linePos + 1)

-- COMMENT HELPERS

oneLineComment :: [Token] -> [Token] -> [LexerError] -> String -> Int -> ([Token], [LexerError])
oneLineComment finishedTokens openerStack errors remainingStr lineNo =
    case remainingStr of
        "" ->
            lexerHelper finishedTokens openerStack errors "" (tail remainingStr) (lineNo + 1) linePosStart
        '\n' : _ ->
            lexerHelper finishedTokens openerStack errors "" (tail remainingStr) (lineNo + 1) linePosStart
        _ ->
            oneLineComment finishedTokens openerStack errors (tail remainingStr) lineNo

multilineComment :: [Token] -> [Token] -> [LexerError] -> String -> Int -> Int -> Int -> Int -> ([Token], [LexerError])
multilineComment finishedTokens openerStack errors remainingStr commentStartLineNo commentStartLinePos lineNo linePos =
    case remainingStr of
        "" ->
            let errorsAddDangler = ((LexerError commentStartLineNo commentStartLinePos DanglingCommentError) : errors)
             in lexerHelper finishedTokens openerStack errorsAddDangler "" remainingStr lineNo linePos
        '*' : '/' : leftover ->
            lexerHelper finishedTokens openerStack errors "" leftover lineNo (linePos + 2)
        '\n' : _ ->
            multilineComment finishedTokens openerStack errors (tail remainingStr) commentStartLineNo commentStartLinePos (lineNo + 1) linePosStart
        _ ->
            multilineComment finishedTokens openerStack errors (tail remainingStr) commentStartLineNo commentStartLinePos lineNo (linePos + 1)

-- ENCLOSING (){}[] HELPERS

closerMatchesLastOpener :: [Token] -> Char -> Bool
closerMatchesLastOpener openerStack closer =
    if null openerStack
        then False
        else case (tokenCat . head) openerStack of
            OPEN_BRACE -> closer == '}'
            OPEN_PAREN -> closer == ')'
            OPEN_BRACK -> closer == ']'

-- DELIM HELPERS

isDelim :: String -> Bool
isDelim s = 
    if null s
        then error (compilerError "Expected delimiter at EOF.")
        else elem (head s) whitespaceDelims || Maybe.isJust (classifyDelim s)

whitespaceDelims :: String
whitespaceDelims = " \n\t\v\r\f"

classifyDelim :: String -> Maybe (TokenCategory, Int)
classifyDelim remainingStr =
    F.asum
        [ case reservedTripleTok remainingStr of
            Just t -> Just (t, 3)
            _ -> Nothing
        , case reservedDoubleTok remainingStr of
            Just t -> Just (t, 2)
            _ -> Nothing
        , case reservedCharTok remainingStr of
            Just t -> Just (t, 1)
            _ -> Nothing
        ]

reservedTripleTok :: String -> Maybe TokenCategory
reservedTripleTok s = 
    case s of
        '<' : '<' : '=' : _ -> Just LEFT_LEFT_EQ
        '>' : '>' : '=' : _ -> Just RIGHT_RIGHT_EQ
        _ -> Nothing

reservedDoubleTok :: String -> Maybe TokenCategory
reservedDoubleTok s =
    case s of
        '<' : '<' : _ -> Just LEFT_LEFT
        '>' : '>' : _ -> Just RIGHT_RIGHT
        '<' : '=' : _ -> Just LEFT_EQ
        '>' : '=' : _ -> Just RIGHT_EQ
        '=' : '=' : _ -> Just EQ_EQ
        '!' : '=' : _ -> Just EXCL_EQ
        '&' : '&' : _ -> Just AMP_AMP
        '|' : '|' : _ -> Just PIPE_PIPE
        '+' : '=' : _ -> Just PLUS_EQ
        '-' : '=' : _ -> Just DASH_EQ
        '*' : '=' : _ -> Just STAR_EQ
        '/' : '=' : _ -> Just SLASH_EQ
        '%' : '=' : _ -> Just PERC_EQ
        '&' : '=' : _ -> Just AMP_EQ
        '^' : '=' : _ -> Just CARET_EQ
        '|' : '=' : _ -> Just PIPE_EQ
        '+' : '+' : _ -> Just PLUS_PLUS
        '-' : '-' : _ -> Just DASH_DASH
        _ -> Nothing

reservedCharTok :: String -> Maybe TokenCategory
reservedCharTok s =
    case s of
        ';' : _ -> Just SEMICOLON
        '(' : _ -> Just OPEN_PAREN
        ')' : _ -> Just CLOSE_PAREN
        '[' : _ -> Just OPEN_BRACK
        ']' : _ -> Just CLOSE_BRACK
        '{' : _ -> Just OPEN_BRACE
        '}' : _ -> Just CLOSE_BRACE
        '+' : _ -> Just PLUS
        '-' : _ -> Just DASH
        '*' : _ -> Just STAR
        '/' : _ -> Just SLASH
        '%' : _ -> Just PERC
        '=' : _ -> Just EQUAL
        '!' : _ -> Just EXCL
        '~' : _ -> Just TILDE
        '<' : _ -> Just LEFT
        '>' : _ -> Just RIGHT
        '&' : _ -> Just AMP
        '^' : _ -> Just CARET
        '|' : _ -> Just PIPE
        '?' : _ -> Just QUEST
        ':' : _ -> Just COLON
        _ -> Nothing

classifyToken :: String -> Maybe TokenCategory
classifyToken s =
    F.asum
        [ case reservedKeywordTok s of
            Just t -> Just t
            _ -> Nothing
        , case decnumTok s of
            Just t -> Just t
            _ -> Nothing
        , case hexnumTok s of
            Just t -> Just t
            _ -> Nothing
        , case identifierTok s of
            Just t -> Just t
            _ -> Nothing
        ]

reservedKeywordTok :: String -> Maybe TokenCategory
reservedKeywordTok s =
    case s of
        "while" -> Just WHILE
        "for" -> Just FOR
        "continue" -> Just CONTINUE
        "break" -> Just BREAK
        "return" -> Just RETURN
        "assert" -> Just ASSERT
        "true" -> Just TRUE
        "false" -> Just FALSE
        "NULL" -> Just NULL
        "alloc" -> Just ALLOC
        "alloc_array" -> Just ALLOC_ARRAY
        "int" -> Just INT
        "bool" -> Just BOOL
        "void" -> Just VOID
        "char" -> Just CHAR
        "string" -> Just STRING
        _ -> Nothing

decnumTok :: String -> Maybe TokenCategory
decnumTok s =
    case s of
        "0" -> Just (DECNUM s)
        f : _
            | (f /= '0')
                && (all C.isDigit s) ->
                Just (DECNUM s)
        _ -> Nothing

hexnumTok :: String -> Maybe TokenCategory
hexnumTok s =
    case s of
        '0' : x : rest
            | (x == 'x' || x == 'X')
                && (all (\c -> C.isDigit c || elem c "abcdefABCDEF") rest) ->
                Just (HEXNUM s)
        _ -> Nothing

identifierTok :: String -> Maybe TokenCategory
identifierTok s =
    let alphaUnder = \c -> C.isLetter c || c == '_'
     in case s of
            f : _
                | (alphaUnder f)
                    && (all (\c -> alphaUnder c || C.isDigit c) s) ->
                    Just (IDENTIFIER s)
            _ -> Nothing
