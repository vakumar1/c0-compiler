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
    bfsSuccessors,
    bfsPredecessors,
)
where

import Types

import qualified Data.Map as Map
import qualified Data.Set as Set

data FunctionIr = FunctionIr
    { functionIrBlocks :: Map.Map Int BasicBlockIr
    , functionIrPredecessorMap :: Map.Map Int (Set.Set Int)
    , functionIrSuccessorMap :: Map.Map Int (Set.Set Int)
    , functionIrTerminators :: Set.Set Int
    }
    deriving (Show)

data BasicBlockIr = BasicBlockIr
    { bbIndex :: Int
    , bbIrCommands :: [CommandIr]
    }
instance Show BasicBlockIr where
    show bb = 
        foldr (\comm interStr -> interStr ++ (show comm) ++ "\n") "\n" (bbIrCommands bb)

data CommandIr
    = INIT_IR VariableIr
    | ASN_PURE_IR VariableIr PureIr
    | ASN_IMPURE_IR VariableIr ImpureIr
    | GOTO_BB_IR Int
    | RET_PURE_IR PureIr
    | PHI_FN_IR String (Map.Map Int String)
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
        newTerminators =
            foldr
                (\bb interSet -> if (bbTerminates bb) then (Set.insert (bbIndex bb) interSet) else interSet)
                (functionIrTerminators fn)
                bbs
     in FunctionIr newBlocks (functionIrPredecessorMap fn) (functionIrSuccessorMap fn) newTerminators

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
     in FunctionIr (functionIrBlocks fn) newPredsMap newSuccsMap (functionIrTerminators fn)

appendCommsToBb :: BasicBlockIr -> [CommandIr] -> BasicBlockIr
appendCommsToBb bb comms = BasicBlockIr (bbIndex bb) (comms ++ (bbIrCommands bb))

bbTerminates :: BasicBlockIr -> Bool
bbTerminates bb =
    case (bbIrCommands bb) of
        [] -> False
        RET_PURE_IR _ : _ -> True
        _ -> False

-- returns a BFS iteration over the CFG starting from block 0
bfsSuccessors :: FunctionIr -> [Int]
bfsSuccessors fnIr = reverse (bfsSuccessorsHelper fnIr [0] (Set.insert 0 Set.empty) [])

bfsSuccessorsHelper :: FunctionIr -> [Int] -> Set.Set Int -> [Int] -> [Int]
bfsSuccessorsHelper fnIr queue seen order =
    case queue of
        [] -> order
        index : _ ->
            let succIds =
                    case Map.lookup index (functionIrSuccessorMap fnIr) of
                        Just s -> s
                        Nothing -> Set.empty
                (newQueue, newSeen) =
                    foldr
                        ( \succId (interQueue, interSeen) ->
                            if (Set.member succId interSeen)
                                then (interQueue, interSeen)
                                else (interQueue ++ [succId], Set.insert succId interSeen)
                        )
                        (tail queue, seen)
                        succIds
                newOrder = index : order
             in bfsSuccessorsHelper fnIr newQueue newSeen newOrder

-- returns a BFS iteration over the CFG starting from the return blocks
bfsPredecessors :: FunctionIr -> [Int]
bfsPredecessors fnIr =
    let retBlocks = bfsPredecessorsInitQueue fnIr
     in reverse (bfsPredecessorsHelper fnIr retBlocks (Set.fromList retBlocks) [])

bfsPredecessorsInitQueue :: FunctionIr -> [Int]
bfsPredecessorsInitQueue fnIr =
    map
        (\(index, bb) -> index)
        (filter (\(index, bb) -> (bbTerminates bb)) (Map.toList (functionIrBlocks fnIr)))

bfsPredecessorsHelper :: FunctionIr -> [Int] -> Set.Set Int -> [Int] -> [Int]
bfsPredecessorsHelper fnIr queue seen order =
    case queue of
        [] -> order
        index : _ ->
            let predIds =
                    case Map.lookup index (functionIrPredecessorMap fnIr) of
                        Just s -> s
                        Nothing -> Set.empty
                (newQueue, newSeen) =
                    foldr
                        ( \predId (interQueue, interSeen) ->
                            if ((Set.member predId interSeen) || (not (allSuccessorsVisited fnIr predId interSeen)))
                                then (interQueue, interSeen)
                                else (interQueue ++ [predId], Set.insert predId interSeen)
                        )
                        (tail queue, seen)
                        predIds
                newOrder = index : order
             in bfsPredecessorsHelper fnIr newQueue newSeen newOrder

allSuccessorsVisited :: FunctionIr -> Int -> Set.Set Int -> Bool
allSuccessorsVisited fnIr predId seen =
    let succIds =
            case Map.lookup predId (functionIrSuccessorMap fnIr) of
                Just s -> s
                Nothing -> Set.empty
     in all (\succId -> Set.member succId seen) succIds
