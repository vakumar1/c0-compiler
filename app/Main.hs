module Main (main) where

import Errors
import Lexer
import Tokens

import System.Environment

handleLexerResult :: ([Token], [LexerError]) -> IO ()
handleLexerResult (tokens, errors) =
    if length errors == 0
        then putStrLn "Lexed"
        else
            error
                ( foldl
                    ( \s e ->
                        s ++ (show e) ++ "\n"
                    )
                    "Lexer Error(s) occured: "
                    errors
                )

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename : _) -> do
            code <- readFile filename
            (handleLexerResult . lexer) code
        _ -> putStrLn "Usage: programname filename"
