module Main (main) where

import Ast
import Errors
import Lexer
import Parser
import Tokens

import System.Environment

handleLexerErrors :: [LexerError] -> IO ()
handleLexerErrors errors =
    putStrLn
        ( foldl
            ( \s e ->
                s ++ (show e) ++ "\n"
            )
            "Lexer Error(s) occured: "
            errors
        )

handleParserErrors :: [ParserError] -> IO ()
handleParserErrors errors =
    putStrLn
        ( foldl
            ( \s e ->
                s ++ (show e) ++ "\n"
            )
            "Parser Error(s) occured: "
            errors
        )

compiler :: String -> IO ()
compiler code =
    let (tokens, lexerErrors) = lexer code
        (exp, parserErrors) = parser tokens
     in if (length lexerErrors) /= 0
            then handleLexerErrors lexerErrors
            else handleParserErrors parserErrors

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename : _) -> do
            code <- readFile filename
            compiler code
        _ -> putStrLn "Usage: programname filename"
