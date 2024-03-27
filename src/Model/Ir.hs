module Model.Ir (
    FunctionIr (..),
    BasicBlockIr (..),
    CommandIr (..),
    PhiFnIr (..),
    PureIr (..),
    PureBaseIr (..),
    PureBinopIr (..),
    PureBinopCatIr (..),
    PureUnopIr (..),
    PureUnopCatIr (..),
    dummyPureIr,
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

import Model.Types

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
    , bbIrPhiFn :: PhiFnIr
    , bbIrCommands :: [CommandIr]
    }
instance Show BasicBlockIr where
    show bb =
        "\nINDEX=" ++ (show (bbIndex bb)) ++
        "\nPHI-FN=" ++ (show (bbIrPhiFn bb)) ++ 
        "\nCOMMANDS=" ++
        foldr 
            (\comm interStr -> 
                interStr ++ "\n" ++ (show comm)
            ) 
            "" 
            (bbIrCommands bb)
            ++
        "\n"

data CommandIr
    = INIT_IR VariableIr
    | ASN_PURE_IR VariableIr PureIr
    | ASN_IMPURE_IR VariableIr ImpureIr
    | GOTO_BB_IR Int
    | SPLIT_BB_IR PureIr Int Int
    | RET_PURE_IR PureIr
    deriving (Show)

type PhiFnIr = Map.Map VariableIr (Map.Map Int VariableIr)

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
    -- arithmetic ops
    = ADD_IR
    | SUB_IR
    | MUL_IR
    | AND_IR
    | XOR_IR
    | OR_IR
    | SLA_IR
    | SRA_IR
    -- boolean ops
    | LT_IR
    | GT_IR
    | LTE_IR
    | GTE_IR
    | EQ_IR
    | NEQ_IR
    | LOGAND_IR
    | LOGOR_IR
    deriving (Show)

data PureUnopIr = PureUnopIr
    { pureUnopCatIr :: PureUnopCatIr
    , pureUnopInfType :: TypeCategory
    , pureUnopBaseIr :: PureBaseIr
    }
    deriving (Show)

data PureUnopCatIr
    = NEG_IR
    | NOT_IR
    | LOGNOT_IR
    deriving (Show)

dummyPureIr :: PureIr
dummyPureIr = PURE_BASE_IR (CONST_IR (BOOL_CONST False))

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
    , variableIrSSAId :: Int
    , variableIrType :: TypeCategory
    , variableIrTemp :: Bool
    }
    deriving (Show)
instance Eq VariableIr where
    var1 == var2 = ((variableIrName var1) == (variableIrName var2)) 
                        && ((variableIrSSAId var1) == (variableIrSSAId var2))
instance Ord VariableIr where
    var1 <= var2 = ((variableIrName var1) < (variableIrName var2))
                        || (((variableIrName var1) == (variableIrName var2)) && ((variableIrSSAId var1) <= (variableIrSSAId var2)))

-- HELPERS
-- adds basic block to function's collection
-- records whether the basic block definitely terminates (ends in return)
addBbsToFunction :: FunctionIr -> [BasicBlockIr] -> FunctionIr
addBbsToFunction fn bbs =
    let newBlocks = foldr (\bb interMap -> Map.insert (bbIndex bb) bb interMap) (functionIrBlocks fn) bbs
        newTerminators =
            foldr
                (\bb interSet -> if (bbTerminates bb) then (Set.insert (bbIndex bb) interSet) else interSet)
                (functionIrTerminators fn)
                bbs
     in FunctionIr newBlocks (functionIrPredecessorMap fn) (functionIrSuccessorMap fn) newTerminators

-- adds edge to function's CFG
-- * may add loops in the CFG in the event of while statements
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
appendCommsToBb bb comms = BasicBlockIr (bbIndex bb) (bbIrPhiFn bb) (comms ++ (bbIrCommands bb))

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
