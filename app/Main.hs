module Main (main) where

-- import Ast
import Errors
import ElabToIr
import Lexer
import AstToElab
import Parser
import Ir
import IrToSSA

-- import Parser
import qualified Data.Map as Map
import qualified Data.Set as Set
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
        ast = parser (reverse tokens)
        elaborated = elaborate ast
        (ir, verErrors) = irFunction elaborated
     in case verErrors of
        [] ->
            let maxSSAIr = (irToMaximalSSA ir)
            in (putStrLn (
                "Blocks\n" ++ ((prettyPrintList . Map.toList . functionIrBlocks) maxSSAIr) ++ "\n" ++ 
                "Predecessors\n" ++ ((prettyPrintList . Map.toList . functionIrPredecessorMap) maxSSAIr) ++ "\n" ++ 
                "Successors\n" ++ ((prettyPrintList . Map.toList . functionIrSuccessorMap) maxSSAIr) ++ "\n" ++
                "Terminators\n" ++ ((prettyPrintList . Set.toList . functionIrTerminators) maxSSAIr)
                ))
        _ -> (putStrLn (
            "ERRORS\n" ++ (prettyPrintList verErrors)
            ))
main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename : _) -> do
            code <- readFile filename
            compiler code
        _ -> putStrLn "Usage: programname filename"
