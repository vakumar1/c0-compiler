module Frontend.Lexer (
    lexer,
) where

import Common.Errors
import Common.Constants
import Model.Tokens
import Model.Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Debug.Trace as Trace
import qualified Text.Show.Pretty as Pretty

lineNoStart :: Int
lineNoStart = 1
linePosStart :: Int
linePosStart = 1

lexer :: String -> ([Token], [LexerError], TypeAliasContext)
lexer code = lexerHelper [] [] [] "" (code ++ ['\n']) emptyTypeAliasCtx lineNoStart linePosStart

lexerHelper :: [Token] -> [Token] -> [LexerError] -> String -> String -> TypeAliasContext -> Int -> Int -> ([Token], [LexerError], TypeAliasContext)
lexerHelper finishedTokens openerStack errors currToken remainingStr aliasCtx lineNo linePos
    -- EOF
    | null remainingStr =
        let errorsAddDangler = foldr (\o l -> (LexerError ((tokenLineNo . tokenData) o) ((tokenLinePos . tokenData) o) DANGLING_OPEN_ENCLOSER) : l) errors openerStack
         in (finishedTokens, errorsAddDangler, aliasCtx)
    -- delimiters
    | isDelim remainingStr =
        -- try to process current token and delimiter
        let (delimTokenCat, delimLength) = case classifyDelim remainingStr of
                Just (c, l) -> (Just c, l)
                Nothing -> (Nothing, 1)
            newRemainingStr = (drop delimLength remainingStr)
            (compressedTokens, newTypeAliasContext, processErrs) = 
                case classifyToken currToken aliasCtx of
                    Just tc -> 
                        let uncompressedTokens = Token tc (TokenData lineNo (linePos - (length currToken))) : finishedTokens
                            (compressedTokens, compressionErrCats) = compressTokens delimTokenCat uncompressedTokens aliasCtx
                            (newTypeAliasContext, aliasErrCats) = updateTypeAliasContext newFinishedTokens aliasCtx
                            errs = 
                                map
                                    (\errCat -> LexerError lineNo (linePos - (length currToken)) errCat)
                                    (compressionErrCats ++ aliasErrCats)
                        in (compressedTokens, newTypeAliasContext, errs)
                    Nothing -> 
                        (finishedTokens, aliasCtx, if currToken == "" then [] else [LexerError lineNo (linePos - (length currToken)) INVALID_TOKEN])
            newFinishedTokens = 
                case delimTokenCat of
                    Just c -> (Token c (TokenData lineNo linePos)) : compressedTokens
                    Nothing -> compressedTokens
            newErrors = processErrs ++ errors
         in case remainingStr of
                -- comment: immediately go into comment mode
                '/' : '*' : _ ->
                    multilineComment compressedTokens openerStack newErrors (drop 2 remainingStr) newTypeAliasContext lineNo linePos lineNo linePos
                '/' : '/' : _ ->
                    oneLineComment compressedTokens openerStack newErrors (drop 2 remainingStr) newTypeAliasContext lineNo
                -- whitespace
                '\n' : _ ->
                    lexerHelper compressedTokens openerStack newErrors "" newRemainingStr newTypeAliasContext (lineNo + 1) 0
                _
                    | elem (head remainingStr) whitespaceDelims ->
                        lexerHelper compressedTokens openerStack newErrors "" newRemainingStr newTypeAliasContext lineNo (linePos + 1)
                -- open enclosure: add delim token to tokens and add opener to stack
                _
                    | elem (head remainingStr) "{[(" ->
                        lexerHelper newFinishedTokens ((head newFinishedTokens) : openerStack) newErrors "" newRemainingStr newTypeAliasContext lineNo (linePos + 1)
                -- close enclousre: validate against stack and try to collapse the enclosure
                _
                    | elem (head remainingStr) "}])" ->
                        if closerMatchesLastOpener openerStack (head remainingStr)
                            then lexerHelper newFinishedTokens (tail openerStack) newErrors "" newRemainingStr newTypeAliasContext lineNo (linePos + 1)
                            else
                                let errorsAddDangler = ((LexerError lineNo linePos DANGLING_CLOSED_ENCLOSER) : newErrors)
                                 in lexerHelper (tail newFinishedTokens) openerStack errorsAddDangler "" newRemainingStr newTypeAliasContext lineNo (linePos + 1)
                -- general delim case: add delim token to tokens
                _ ->
                    lexerHelper newFinishedTokens openerStack newErrors "" newRemainingStr newTypeAliasContext lineNo (linePos + delimLength)
    -- general case
    | otherwise = lexerHelper finishedTokens openerStack errors (currToken ++ [(head remainingStr)]) (tail remainingStr) aliasCtx lineNo (linePos + 1)

-- COMMENT HELPERS

oneLineComment :: [Token] -> [Token] -> [LexerError] -> String -> TypeAliasContext -> Int -> ([Token], [LexerError], TypeAliasContext)
oneLineComment finishedTokens openerStack errors remainingStr aliasCtx lineNo =
    case remainingStr of
        "" ->
            lexerHelper finishedTokens openerStack errors "" (tail remainingStr) aliasCtx (lineNo + 1) linePosStart
        '\n' : _ ->
            lexerHelper finishedTokens openerStack errors "" (tail remainingStr) aliasCtx (lineNo + 1) linePosStart
        _ ->
            oneLineComment finishedTokens openerStack errors (tail remainingStr) aliasCtx lineNo

multilineComment :: [Token] -> [Token] -> [LexerError] -> String -> TypeAliasContext -> Int -> Int -> Int -> Int -> ([Token], [LexerError], TypeAliasContext)
multilineComment finishedTokens openerStack errors remainingStr aliasCtx commentStartLineNo commentStartLinePos lineNo linePos =
    case remainingStr of
        "" ->
            let errorsAddDangler = ((LexerError commentStartLineNo commentStartLinePos DANGLING_COMMENT) : errors)
             in lexerHelper finishedTokens openerStack errorsAddDangler "" remainingStr aliasCtx lineNo linePos
        '*' : '/' : leftover ->
            lexerHelper finishedTokens openerStack errors "" leftover aliasCtx lineNo (linePos + 2)
        '\n' : _ ->
            multilineComment finishedTokens openerStack errors (tail remainingStr) aliasCtx commentStartLineNo commentStartLinePos (lineNo + 1) linePosStart
        _ ->
            multilineComment finishedTokens openerStack errors (tail remainingStr) aliasCtx commentStartLineNo commentStartLinePos lineNo (linePos + 1)

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

forceClassifyDelim :: String -> (TokenCategory, Int)
forceClassifyDelim remainingStr =
    case classifyDelim remainingStr of
        Just c -> c
        Nothing -> error . compilerError $ "Failed to classify delim token: " ++ remainingStr

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
        ',' : _ -> Just COMMA
        _ -> Nothing

classifyToken :: String -> TypeAliasContext -> Maybe TokenCategory
classifyToken s aliasCtx =
    F.asum
        [ case reservedKeywordTok s of
            Just t -> Just t
            _ -> Nothing
        , case typeTok s aliasCtx of
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
        "if" -> Just IF
        "else" -> Just ELSE
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
        "typedef" -> Just TYPEDEF
        "struct" -> Just STRUCT
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

typeTok :: String -> TypeAliasContext -> Maybe TokenCategory
typeTok s aliasCtx = 
    case s of
        "int" -> Just (TYPE INT_TYPE)
        "bool" -> Just (TYPE BOOL_TYPE)
        "void" -> Just (TYPE VOID_TYPE)
        "char" -> Just (TYPE CHAR_TYPE)
        "string" -> Just (TYPE STRING_TYPE)
        _ ->
            case Map.lookup s (typeAliasContextAliases aliasCtx) of
                Just resolved -> Just (TYPE resolved)
                Nothing -> Nothing

identifierTok :: String -> Maybe TokenCategory
identifierTok s =
    let alphaUnder = \c -> C.isLetter c || c == '_'
     in case s of
            f : _
                | (alphaUnder f)
                    && (all (\c -> alphaUnder c || C.isDigit c) s) ->
                    Just (IDENTIFIER s)
            _ -> Nothing

compressTokens :: Maybe TokenCategory -> [Token] -> TypeAliasContext -> ([Token], [LexerErrorCategory])
compressTokens delimTok finishedTokens aliasCtx = 
    case map tokenCat finishedTokens of
        IDENTIFIER structName : STRUCT : _ ->
            -- attempt to resolve struct type if this is a decl or struct is already declared
            if ((Maybe.isJust delimTok && Maybe.fromJust delimTok == SEMICOLON) || Set.member structName (typeAliasContextStructDecls aliasCtx))
                then 
                    let compressTok = Token (TYPE (STRUCT_TYPE structName [])) (tokenData . head $ finishedTokens)
                    in (compressTok : (drop 2 finishedTokens), [])
                else
                    let err = UNDECLARED_STRUCT (UndeclaredStructError (head finishedTokens))
                    in (finishedTokens, [err])
        _ ->
            (finishedTokens, [])

updateTypeAliasContext :: [Token] -> TypeAliasContext -> (TypeAliasContext, [LexerErrorCategory])
updateTypeAliasContext finishedTokens aliasCtx = 
    let finishedTokenCats = map tokenCat finishedTokens
    in case finishedTokenCats of
            -- insert new type alias
            SEMICOLON : IDENTIFIER alias : TYPE target : TYPEDEF : _ ->
                let newCtx = 
                        TypeAliasContext 
                            (Map.insert alias target (typeAliasContextAliases aliasCtx))
                            (typeAliasContextStructs aliasCtx)
                            (typeAliasContextStructDecls aliasCtx)
                    errs = 
                        case Map.lookup alias (typeAliasContextAliases aliasCtx) of
                            Just currTarget -> [DUPLICATE_TYPEDEF_DEFN (DuplicateTypedefDefnError (finishedTokens !! 1) currTarget target)]
                            Nothing -> []
                in (newCtx, errs)
            -- insert new struct decl
            SEMICOLON : TYPE (STRUCT_TYPE structName _) : _ ->
                let newCtx = 
                        TypeAliasContext
                            (typeAliasContextAliases aliasCtx)
                            (typeAliasContextStructs aliasCtx)
                            (Set.insert structName (typeAliasContextStructDecls aliasCtx))
                    errs = []
                in (newCtx, errs)
            -- attempt to insert new struct defn
            _ ->
                case (extractStructDefn finishedTokenCats) of
                    Just (STRUCT_TYPE structName structFields) ->
                        let newCtx = 
                                TypeAliasContext
                                    (typeAliasContextAliases aliasCtx)
                                    (Map.insert structName (STRUCT_TYPE structName structFields) (typeAliasContextStructs aliasCtx))
                                    (Set.insert structName (typeAliasContextStructDecls aliasCtx))
                            errs = 
                                case Map.lookup structName (typeAliasContextStructs aliasCtx) of
                                    Just currTarget -> [DUPLICATE_STRUCT_DEFN (DuplicateStructDefnError currTarget (STRUCT_TYPE structName structFields))]
                                    Nothing -> []
                        in (newCtx, errs)
                    _ -> 
                        (aliasCtx, [])

extractStructDefn :: [TokenCategory] -> Maybe TypeCategory
extractStructDefn finishedTokens = 
    case finishedTokens of
        SEMICOLON : tailTokens -> extractStructDefnFields tailTokens []
        _ -> Nothing

extractStructDefnFields :: [TokenCategory] -> [(String, TypeCategory)] -> Maybe TypeCategory
extractStructDefnFields remainingTokens currFields = 
    case remainingTokens of
        OPEN_BRACE : TYPE (STRUCT_TYPE structName _) : _ -> Just (STRUCT_TYPE structName currFields)
        SEMICOLON : IDENTIFIER fieldName : TYPE fieldType : tailTokens -> extractStructDefnFields tailTokens ((fieldName, fieldType) : currFields)
        _ -> Nothing
