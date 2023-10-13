module Main (main) where

import Ast
import Errors
import Lexer
import Parser
import Tokens

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

compiler :: String -> IO ()
compiler code =
    let (tokens, lexerErrors) = lexer code
        (programExp, parserErrors) = parser tokens
     in if (length lexerErrors) /= 0
            then putStrLn ((handleLexerErrors lexerErrors) ++ "\n\n" ++ (show tokens))
            else putStrLn ((handleParserErrors parserErrors) ++ "\n\n" ++ (show programExp))

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename : _) -> do
            code <- readFile filename
            compiler code
        _ -> putStrLn "Usage: programname filename"
