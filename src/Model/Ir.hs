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
    variableIrBaseEq,
    addBbsToFunction,
    addEdgeToCFG,
    appendCommsToBb,
    bbTerminates,
    looseBbOrdering,
)
where

import Model.Types
import Common.Graphs

import qualified Data.Map as Map
import qualified Data.Set as Set

data FunctionIr = FunctionIr
    { functionIrBlocks :: Map.Map Int BasicBlockIr
    , functionIrCFG :: DirectedGraph Int
    }
    deriving (Show)

data BasicBlockIr = BasicBlockIr
    { bbIndex :: Int
    , bbIrPhiFn :: PhiFnIr
    , bbIrCommands :: [CommandIr]
    }
    deriving (Show)

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
    | SAL_IR
    | SAR_IR
    -- boolean ops
    | LT_IR
    | GT_IR
    | LTE_IR
    | GTE_IR
    | EQ_IR
    | NEQ_IR
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
variableIrBaseEq :: VariableIr -> VariableIr -> Bool
variableIrBaseEq var1 var2 = 
    (variableIrName var1) == (variableIrName var2)

-- HELPERS
-- adds basic block to function's collection
-- records whether the basic block definitely terminates (ends in return)
addBbsToFunction :: [BasicBlockIr] -> FunctionIr -> FunctionIr
addBbsToFunction bbs fn =
    let newBlocks = 
            foldr
                (\bb interBlocks ->
                    Map.insert (bbIndex bb) bb interBlocks
                )
                (functionIrBlocks fn)
                bbs
        newCFG = 
            foldr 
                addNode 
                (functionIrCFG fn) 
                (map bbIndex bbs)
     in FunctionIr newBlocks newCFG

-- adds edge from source->dest to function's CFG
-- * may add loops in the CFG in the event of while statements
addEdgeToCFG :: Int -> Int -> FunctionIr -> FunctionIr
addEdgeToCFG source dest fn =
    let newCFG = addEdge source dest (functionIrCFG fn)
    in FunctionIr (functionIrBlocks fn) (addEdge source dest (functionIrCFG fn))

appendCommsToBb :: BasicBlockIr -> [CommandIr] -> BasicBlockIr
appendCommsToBb bb comms = BasicBlockIr (bbIndex bb) (bbIrPhiFn bb) (comms ++ (bbIrCommands bb))

bbTerminates :: BasicBlockIr -> Bool
bbTerminates bb =
    case (bbIrCommands bb) of
        [] -> False
        RET_PURE_IR _ : _ -> True
        _ -> False

-- returns a loose BFS ordering of BB indices for purposes of clarity 
-- (where an exact ordering is desired but unneeded)
looseBbOrdering :: FunctionIr -> [Int]
looseBbOrdering fnIr = 
    looseBbOrderingHelper (Set.singleton 0) Set.empty fnIr []

looseBbOrderingHelper :: Set.Set Int -> Set.Set Int -> FunctionIr -> [Int] -> [Int]
looseBbOrderingHelper queue seen fnIr currOrder =
    if Set.size queue == 0
        then currOrder
        else
            let newSeen = Set.union queue seen
                newQueue = 
                    foldr
                        (\bbIndex interQueue ->
                            let succIndices = 
                                    case Map.lookup bbIndex (graphSuccessors (functionIrCFG fnIr)) of
                                        Just s -> s
                                        Nothing -> Set.empty
                                unseenSuccIndices = 
                                    Set.filter
                                        (\succIndex ->
                                            not (Set.member succIndex seen)    
                                        )
                                        succIndices
                            in Set.union interQueue unseenSuccIndices
                        )
                        Set.empty
                        queue
                newOrder = currOrder ++ (Set.toList queue)
            in looseBbOrderingHelper newQueue newSeen fnIr newOrder
