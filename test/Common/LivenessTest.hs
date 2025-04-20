module Common.LivenessTest (
    liveness_test,
) where

import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set

import Model.Ir
import Model.Types
import Common.Aliasing
import Common.Graphs
import Common.Liveness
import Common.Constants

liveness_test :: IO ()
liveness_test = hspec $ do
    describe "PHI function liveness analysis" $ do
        describe "getAssignedVarsPhi" $ do
            it "extracts assigned variables from a phi function" $ do
                let var1 = VariableIr "x" 1 INT_TYPE False
                    var2 = VariableIr "y" 2 INT_TYPE False
                    phiFn = Map.fromList [(var1, Map.empty), (var2, Map.empty)]
                getAssignedVarsPhi phiFn `shouldBe` Set.fromList [var1, var2]
        
        describe "getUsedVarsPredMap" $ do
            it "extracts used variables from a predecessor map" $ do
                let var1 = VariableIr "x" 0 INT_TYPE False
                    var2 = VariableIr "x" 0 INT_TYPE False
                    predMap = Map.fromList [(1, var1), (2, var2)]
                
                getUsedVarsPredMap predMap `shouldBe` Set.fromList [var1, var2]
        
        describe "getUsedVarsPhi" $ do
            it "extracts used variables from a phi function" $ do
                let var1 = VariableIr "x" 1 INT_TYPE False
                    var2 = VariableIr "x" 0 INT_TYPE False
                    var3 = VariableIr "x" 2 INT_TYPE False
                    phiFn = Map.fromList [
                        (var1, Map.fromList [(1, var2), (2, var3)])
                      ]
                getUsedVarsPhi phiFn `shouldBe` Set.fromList [var2, var3]

        describe "updateLiveVarsPhi" $ do
            it "removes assigned vars and adds used vars to live set" $ do
                let liveVar = VariableIr "a" 0 INT_TYPE False
                    assignedVar = VariableIr "x" 1 INT_TYPE False
                    usedVar1 = VariableIr "x" 0 INT_TYPE False
                    usedVar2 = VariableIr "x" 2 INT_TYPE False
                    phiFn = Map.fromList [
                        (assignedVar, Map.fromList [(1, usedVar1), (2, usedVar2)])
                      ]
                updateLiveVarsPhi (Set.fromList [liveVar, assignedVar]) phiFn `shouldBe` Set.fromList [liveVar, usedVar1, usedVar2]

    describe "Command liveness analysis" $ do
        describe "getUsedVarsCommand" $ do
            it "includes the source variable" $ do
                let var = VariableIr "x" 0 INT_TYPE False
                    usedVar = VariableIr "y" 0 INT_TYPE False
                    pure = PURE_BASE_IR (VAR_IR usedVar)
                    cmd = ASN_PURE_IR emptyMemopIr var pure
                getUsedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Set.fromList [usedVar]
            
            it "includes dereferenced destination and offset variables in memop" $ do
                let var = VariableIr "x" 0 (POINTER_TYPE INT_TYPE) False
                    usedVar = VariableIr "y" 0 INT_TYPE False
                    offsetVar = VariableIr "offset" 0 INT_TYPE False
                    memop = MemopIr True (Just (VAR_IR offsetVar)) INT_TYPE
                    pure = PURE_BASE_IR (VAR_IR usedVar)
                    cmd = ASN_PURE_IR memop var pure
                getUsedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Set.fromList [var, usedVar, offsetVar]
            
            it "extracts used variables from ASN_IMPURE_IR" $ do
                let var = VariableIr "x" 0 INT_TYPE False
                    arg1 = VariableIr "a" 0 INT_TYPE False
                    arg2 = VariableIr "b" 0 INT_TYPE False
                    impure = IMPURE_BINOP_IR (ImpureBinopIr DIV_IR INT_TYPE (VAR_IR arg1) (VAR_IR arg2))
                    cmd = ASN_IMPURE_IR var impure
                getUsedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Set.fromList [arg1, arg2]
            
            it "extracts used variables from SPLIT_BB_IR" $ do
                let var = VariableIr "cond" 0 BOOL_TYPE False
                    pure = PURE_BASE_IR (VAR_IR var)
                    cmd = SPLIT_BB_IR pure 1 2
                getUsedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Set.singleton var
                
            it "extracts used variables from RET_PURE_IR" $ do
                let var = VariableIr "ret" 0 INT_TYPE False
                    pure = PURE_BASE_IR (VAR_IR var)
                    cmd = RET_PURE_IR pure
                getUsedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Set.singleton var
                
            it "returns empty set for explicitly stack-allocated variables" $ do
                let var = VariableIr "ret" 0 INT_TYPE False
                    pure = PURE_BASE_IR (VAR_IR var)
                    cmd = RET_PURE_IR pure
                getUsedVarsCommand (AliasingCtx (Set.singleton var)) cmd `shouldBe` Set.empty

            it "returns empty set for commands with no used variables" $ do
                getUsedVarsCommand (AliasingCtx Set.empty) (INIT_IR (VariableIr "x" 0 INT_TYPE False)) `shouldBe` Set.empty
                getUsedVarsCommand (AliasingCtx Set.empty) (GOTO_BB_IR 1) `shouldBe` Set.empty
                getUsedVarsCommand (AliasingCtx Set.empty) RET_IR `shouldBe` Set.empty
                getUsedVarsCommand (AliasingCtx Set.empty) ABORT_IR `shouldBe` Set.empty
                
        describe "getAssignedVarsCommand" $ do
            it "returns assigned variable from ASN_PURE_IR with no dereference" $ do
                let var = VariableIr "x" 0 INT_TYPE False
                    pure = PURE_BASE_IR (CONST_IR (INT_CONST 42))
                    cmd = ASN_PURE_IR emptyMemopIr var pure
                getAssignedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Just var
                
            it "returns Nothing for ASN_PURE_IR with dereference" $ do
                let var = VariableIr "x" 0 (POINTER_TYPE INT_TYPE) False
                    memop = MemopIr True Nothing INT_TYPE
                    pure = PURE_BASE_IR (CONST_IR (INT_CONST 42))
                    cmd = ASN_PURE_IR memop var pure
                getAssignedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Nothing
                
            it "returns Nothing for ASN_PURE_IR with offset" $ do
                let var = VariableIr "x" 0 (POINTER_TYPE INT_TYPE) False
                    offsetVar = VariableIr "offset" 0 INT_TYPE False
                    memop = MemopIr False (Just (VAR_IR offsetVar)) INT_TYPE
                    pure = PURE_BASE_IR (CONST_IR (INT_CONST 42))
                    cmd = ASN_PURE_IR memop var pure
                getAssignedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Nothing

            it "returns Nothing for explicitly stack-allocated ASN_PURE_IR" $ do
                let var = VariableIr "x" 0 (POINTER_TYPE INT_TYPE) False
                    memop = emptyMemopIr
                    pure = PURE_BASE_IR (CONST_IR (INT_CONST 42))
                    cmd = ASN_PURE_IR memop var pure
                getAssignedVarsCommand (AliasingCtx (Set.singleton var)) cmd `shouldBe` Nothing
                
            it "returns assigned variable from ASN_IMPURE_IR" $ do
                let var = VariableIr "x" 0 INT_TYPE False
                    impure = IMPURE_BINOP_IR (ImpureBinopIr DIV_IR INT_TYPE 
                                              (CONST_IR (INT_CONST 10)) 
                                              (CONST_IR (INT_CONST 2)))
                    cmd = ASN_IMPURE_IR var impure
                getAssignedVarsCommand (AliasingCtx Set.empty) cmd `shouldBe` Just var
                
            it "returns Nothing for commands with no assignment" $ do
                getAssignedVarsCommand (AliasingCtx Set.empty) (INIT_IR (VariableIr "x" 0 INT_TYPE False)) `shouldBe` Nothing
                getAssignedVarsCommand (AliasingCtx Set.empty) (GOTO_BB_IR 1) `shouldBe` Nothing
                getAssignedVarsCommand (AliasingCtx Set.empty) (SPLIT_BB_IR dummyPureIr 1 2) `shouldBe` Nothing
                getAssignedVarsCommand (AliasingCtx Set.empty) (RET_PURE_IR dummyPureIr) `shouldBe` Nothing
                getAssignedVarsCommand (AliasingCtx Set.empty) RET_IR `shouldBe` Nothing
                getAssignedVarsCommand (AliasingCtx Set.empty) ABORT_IR `shouldBe` Nothing
        
        describe "updateLiveVarsComm" $ do
            it "adds used vars and removes assigned var" $ do
                let var = VariableIr "x" 0 INT_TYPE False
                    usedVar = VariableIr "y" 0 INT_TYPE False
                    otherVar = VariableIr "z" 0 INT_TYPE False
                    pure = PURE_BASE_IR (VAR_IR usedVar)
                    cmd = ASN_PURE_IR emptyMemopIr var pure
                updateLiveVarsComm (AliasingCtx Set.empty) (Set.fromList [var, otherVar]) cmd `shouldBe` Set.fromList [usedVar, otherVar]
            
            it "handles SPLIT_BB_IR correctly" $ do
                let condVar = VariableIr "cond" 0 BOOL_TYPE False
                    otherVar = VariableIr "other" 0 INT_TYPE False
                    pure = PURE_BASE_IR (VAR_IR condVar)
                    cmd = SPLIT_BB_IR pure 1 2
                updateLiveVarsComm (AliasingCtx Set.empty) (Set.fromList [otherVar]) cmd `shouldBe` Set.fromList [condVar, otherVar]

            it "preserves live vars for commands with no effect on liveness" $ do
                let liveVars = Set.fromList [VariableIr "x" 0 INT_TYPE False]
                    cmd = GOTO_BB_IR 1
                updateLiveVarsComm (AliasingCtx Set.empty) liveVars cmd `shouldBe` liveVars

    describe "LivenessPass integration" $ do
        it "handles basic function with single block and arguments" $ do
            let arg = VariableIr "arg" 0 INT_TYPE False
                ret = VariableIr "ret" 0 INT_TYPE False
                
                commands = [
                    RET_PURE_IR (PURE_BASE_IR (VAR_IR ret)),
                    ASN_PURE_IR emptyMemopIr ret (PURE_BASE_IR (VAR_IR arg))
                  ]
                
                bb = BasicBlockIr "test" 0 Map.empty commands
                bbs = Map.singleton 0 bb
                cfg = addNode 0 emptyGraph
                fnIr = FunctionIr "test" [arg] bbs cfg
                tarjanResult = tarjansAlgo 0 cfg

                expected = Map.singleton 0 (Set.singleton arg)
            
            livenessPass fnIr tarjanResult (AliasingCtx Set.empty) `shouldBe` expected
        
        it "handles function with multiple blocks and loops" $ do
            -- Create a simple loop with two blocks
            let arg = VariableIr "n" 0 INT_TYPE False
                i_0 = VariableIr "i" 0 INT_TYPE False
                i_1 = VariableIr "i" 1 INT_TYPE False
                i_2 = VariableIr "i" 2 INT_TYPE False
                one = VariableIr "one" 0 INT_TYPE False
                cond = VariableIr "cond" 0 BOOL_TYPE False
                
                -- Block 0: Entry
                cmd0 = [
                    GOTO_BB_IR 1,
                    ASN_PURE_IR emptyMemopIr i_0 (PURE_BASE_IR (CONST_IR (INT_CONST 0))),
                    ASN_PURE_IR emptyMemopIr one (PURE_BASE_IR (CONST_IR (INT_CONST 1)))
                  ]
                bb0 = BasicBlockIr "test" 0 Map.empty cmd0
                
                -- Block 1: Loop header (with phi)
                phi1 = Map.fromList [(i_1, Map.fromList [(0, i_0), (2, i_2)])]
                cmd1 = [
                    SPLIT_BB_IR (PURE_BASE_IR (VAR_IR cond)) 2 3,
                    ASN_PURE_IR emptyMemopIr cond (PURE_BINOP_IR (PureBinopIr LT_IR BOOL_TYPE 
                                                                 (VAR_IR i_1) (VAR_IR arg)))
                  ]
                bb1 = BasicBlockIr "test" 1 phi1 cmd1
                
                -- Block 2: Loop body
                cmd2 = [
                    GOTO_BB_IR 1,
                    ASN_PURE_IR emptyMemopIr i_2 (PURE_BINOP_IR (PureBinopIr ADD_IR INT_TYPE 
                                                               (VAR_IR i_1) (VAR_IR one)))
                  ]
                bb2 = BasicBlockIr "test" 2 Map.empty cmd2
                
                -- Block 3: Exit
                cmd3 = [
                    RET_PURE_IR (PURE_BASE_IR (VAR_IR i_1))
                  ]
                bb3 = BasicBlockIr "test" 3 Map.empty cmd3
                
                -- Build function
                bbs = Map.fromList [(0, bb0), (1, bb1), (2, bb2), (3, bb3)]
                cfg = addEdge 0 1 $ addEdge 1 2 $ addEdge 1 3 $ addEdge 2 1
                        (foldr addNode emptyGraph [0, 1, 2, 3])
                fnIr = FunctionIr "test" [arg] bbs cfg
                tarjanResult = tarjansAlgo 0 cfg
                
                expected = Map.fromList [
                    (0, Set.fromList [arg, i_2]),
                    (1, Set.fromList [arg, i_0, i_2, one]),
                    (2, Set.fromList [arg, i_0, i_1, one]),
                    (3, Set.singleton i_1)
                  ]
            
            livenessPass fnIr tarjanResult (AliasingCtx Set.empty) `shouldBe` expected