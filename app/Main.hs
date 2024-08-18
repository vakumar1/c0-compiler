module Main (main) where

-- import Ast

import Middleend.AstToElab
import Backend.Codegen
import Middleend.ElabToIr
import Common.Errors
import Common.Graphs
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
import qualified Debug.Trace as Trace
import qualified Text.Show.Pretty as Pretty
import System.Environment

handleLexerErrors :: [LexerError] -> String
handleLexerErrors errors =
    "Lexer Error(s) occurred: " ++ (Pretty.ppShow errors)

handleVerificationErrors :: [VerificationError] -> String
handleVerificationErrors errors =
    "Program Verification Error(s) occurred: " ++ (Pretty.ppShow errors)

compiler :: String -> String
compiler code =
    let (tokens, lexerErrors) = lexer code
        ast = parser (reverse tokens)
        (elaborated, structCtx) = elaborateProg ast
        (ir, verErrors) = irProg "program" structCtx elaborated
        zippedIrColorings = 
            map
                (\fnIr ->
                    let (root, leaves, dag, sccMap) = tarjansAlgo 0 (functionIrCFG fnIr)
                        maxSSAIr = irToMaximalSSA fnIr (root, leaves, dag, sccMap)
                        coloring = regAllocColoring maxSSAIr (root, leaves, dag, sccMap)
                    in (maxSSAIr, coloring)
                )
                ir
        x86inst = zippedProgIrToX86 structCtx zippedIrColorings
        x86asm = concat $ map show x86inst
     in if not (null lexerErrors)
            then error . handleLexerErrors $ lexerErrors
            else
                if not (null verErrors)
                    then error . handleVerificationErrors $ verErrors
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
