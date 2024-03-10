module Main (main) where

-- import Ast

import Middleend.AstToElab
import Backend.Codegen
import Middleend.ElabToIr
import Common.Errors
import Model.Ir
import Middleend.IrToSSA
import Frontend.Lexer
import Common.Liveness
import Frontend.Parser
import Backend.RegAlloc
import Model.X86

-- import Parser
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment

handleLexerErrors :: [LexerError] -> String
handleLexerErrors errors =
    "Lexer Error(s) occurred: " ++ (prettyPrintList errors)

handleParserErrors :: [ParserError] -> String
handleParserErrors errors =
    "Parser Error(s) occurred: \n" ++ (prettyPrintList errors)

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
        x86asm = foldl (\interCode instr -> interCode ++ (show instr)) "" (irToX86 coloring maxSSAIr)
     in if not (null lexerErrors)
            then prettyPrintList lexerErrors
            else
                if not (null verErrors)
                    then prettyPrintList verErrors
                    else x86asm

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename : rem) ->
            let runtimeAsm =
                    case rem of
                        "debug" : _ ->
                            "src/asm/debug_runtime.asm"
                        _ ->
                            "src/asm/runtime.asm"
             in do
                    cCode <- readFile filename
                    runtime <- readFile runtimeAsm
                    putStrLn (runtime ++ (compiler cCode))
        _ -> putStrLn "Usage: programname filename [debug]"
