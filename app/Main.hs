module Main (main) where

-- import Ast
import Errors
import Lexer
import Parser

-- import Parser

import System.Environment

handleLexerErrors :: [LexerError] -> String
handleLexerErrors errors =
    ( foldl
        ( \s e ->
            s ++ (show e) ++ "\n"
        )
        "Lexer Error(s) occurred: "
        errors
    )

handleParserErrors :: [ParserError] -> String
handleParserErrors errors =
    ( foldl
        ( \s e ->
            s ++ (show e) ++ "\n"
        )
        "Parser Error(s) occurred: \n"
        errors
    )

prettyPrintList :: (Show a) => [a] -> String
prettyPrintList l =
    ( foldl
        ( \s e ->
            s ++ (show e) ++ "\n"
        )
        ""
        l
    )

compiler :: String -> IO ()
compiler code =
    let (tokens, lexerErrors) = lexer code
        final = parser (reverse tokens)
    in ((putStrLn . show) final)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename : _) -> do
            code <- readFile filename
            compiler code
        _ -> putStrLn "Usage: programname filename"
