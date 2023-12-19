module Ir (
    FunctionIr (..),
    BasicBlockIr (..),
    CommandIr (..),
    PureIr (..),
    PureBaseIr (..),
    PureBinopIr (..),
    PureBinopCatIr (..),
    PureUnopIr (..),
    PureUnopCatIr (..),
    ImpureIr (..),
    ImpureBinopIr (..),
    ImpureBinopCatIr (..),
    VariableIr (..),
    addBbsToFunction,
    addEdgeToCFG,
    appendCommsToBb,
    bbTerminates,
    bfsSuccessors
)
where

import Types

import qualified Data.Map as Map
import qualified Data.Set as Set

data FunctionIr = FunctionIr
    { functionIrBlocks :: Map.Map Int BasicBlockIr
    , functionIrPredecessorMap :: Map.Map Int (Set.Set Int)
    , functionIrSuccessorMap :: Map.Map Int (Set.Set Int)
    }
    deriving (Show)

data BasicBlockIr = BasicBlockIr
    { bbIndex :: Int
    , bbIrCommands :: [CommandIr]
    }
    deriving (Show)

data CommandIr
    = INIT_IR VariableIr
    | ASN_PURE_IR VariableIr PureIr
    | ASN_IMPURE_IR VariableIr ImpureIr
    | GOTO_BB_IR Int
    | RET_PURE_IR PureIr
    | PHI_FN_IR VariableIr [VariableIr]
    deriving (Show)

-- pure operations
data PureIr
    = PURE_BASE_IR PureBaseIr
    | PURE_BINOP_IR PureBinopIr
    | PURE_UNOP_IR PureUnopIr
    deriving (Show)

data PureBaseIr
    = CONST_IR Const
    | VAR_IR VariableIr
    deriving (Show)

data PureBinopIr = PureBinopIr
    { pureBinopCatIr :: PureBinopCatIr
    , pureBinopInfType :: TypeCategory
    , pureBinopBaseIr1 :: PureBaseIr
    , pureBinopBaseIr2 :: PureBaseIr
    }
    deriving (Show)

data PureBinopCatIr
    = ADD_IR
    | SUB_IR
    | MUL_IR
    deriving (Show)

data PureUnopIr = PureUnopIr
    { pureUnopCatIr :: PureUnopCatIr
    , pureUnopInfType :: TypeCategory
    , pureUnopBaseIr :: PureBaseIr
    }
    deriving (Show)

data PureUnopCatIr
    = NEG_IR 
    deriving (Show)

-- impure operations
data ImpureIr
    = IMPURE_BINOP_IR ImpureBinopIr
    deriving (Show)

data ImpureBinopIr = ImpureBinopIr
    { impureBinopCatIr :: ImpureBinopCatIr
    , impureBinopInfType :: TypeCategory
    , impureBinopBaseIr1 :: PureBaseIr
    , impureBinopBaseIr2 :: PureBaseIr
    }
    deriving (Show)

data ImpureBinopCatIr
    = DIV_IR
    | MOD_IR
    deriving (Show)

data VariableIr = VariableIr
    { variableIrName :: String
    , variableIrType :: TypeCategory
    , variableIrTemp :: Bool
    }
    deriving (Show)

-- HELPERS
addBbsToFunction :: FunctionIr -> [BasicBlockIr] -> FunctionIr
addBbsToFunction fn bbs = 
    let newBlocks = foldr (\bb interMap -> Map.insert (bbIndex bb) bb interMap) (functionIrBlocks fn) bbs
    in FunctionIr newBlocks (functionIrPredecessorMap fn) (functionIrSuccessorMap fn)

addEdgeToCFG :: FunctionIr -> Int -> Int -> FunctionIr
addEdgeToCFG fn source dest = 
    let destPreds = 
            case Map.lookup dest (functionIrPredecessorMap fn) of
                Just s -> s
                Nothing -> Set.empty
        newDestPreds = Set.insert source destPreds
        newPredsMap = Map.insert dest newDestPreds (functionIrPredecessorMap fn)
        srcSuccs = 
            case Map.lookup source (functionIrSuccessorMap fn) of
                Just s -> s
                Nothing -> Set.empty
        newSrcSuccs = Set.insert dest srcSuccs
        newSuccsMap = Map.insert source newSrcSuccs (functionIrSuccessorMap fn)
    in  FunctionIr (functionIrBlocks fn) newPredsMap newSuccsMap

appendCommsToBb :: BasicBlockIr -> [CommandIr] -> BasicBlockIr
appendCommsToBb bb comms = BasicBlockIr (bbIndex bb) (comms ++ (bbIrCommands bb))

bbTerminates :: BasicBlockIr -> Bool
bbTerminates bb = 
    case (bbIrCommands bb) of
        [] -> False
        RET_PURE_IR _ : _ -> True
        _ -> False

-- returns a BFS iteration over the CFG
bfsSuccessors :: FunctionIr -> [Int]
bfsSuccessors fnIr = bfsSuccessorsHelper fnIr [0] Set.empty []

bfsSuccessorsHelper :: FunctionIr -> [Int] -> Set.Set Int -> [Int] -> [Int]
bfsSuccessorsHelper fnIr queue seen order = 
    case queue of
        [] -> order
        id : _ ->
            let succIds = 
                    case Map.lookup id (functionIrSuccessorMap fnIr) of
                        Just s -> s
                        Nothing -> Set.empty
                newQueue = foldr (\succId interQueue -> if (Set.member succId seen) then interQueue else (interQueue ++ [succId])) (tail queue) succIds
                newSeen = foldr (\succId interSeen -> Set.insert succId interSeen) seen succIds
                newOrder = id:order
            in bfsSuccessorsHelper fnIr newQueue newSeen newOrder
