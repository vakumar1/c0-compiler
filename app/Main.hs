module Main (main) where

-- import Ast
import Errors
import ElabToIr
import Lexer
import AstToElab
import Parser
import Ir
import IrToSSA
import Codegen
import X86
import Liveness

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

compiler :: String -> String
compiler code =
    let (tokens, lexerErrors) = lexer code
        ast = parser (reverse tokens)
        elaborated = elaborate ast
        (ir, verErrors) = irFunction elaborated
        maxSSAIr = irToMaximalSSA ir
        coloring = regAllocColoring maxSSAIr
        x86instr = irToX86 coloring maxSSAIr
    in foldr (\instr interCode -> interCode ++ (show instr)) "" x86instr

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename : _) -> do
            cCode <- readFile filename
            putStrLn (compiler cCode)

        _ -> putStrLn "Usage: programname filename"
