module Common.IrUtils (
    getBB,
    addBbsToFunction,
    addEdgeToCFG,
    appendCommsToBb,
    bbTerminates,
    looseBbOrdering,
) 
where

import Common.Errors
import Common.Graphs
import Model.Ir

import qualified Data.Set as Set
import qualified Data.Map as Map

getBB :: FunctionIr -> Int -> BasicBlockIr
getBB fnIr index = 
    case Map.lookup index (functionIrBlocks fnIr) of
        Just bb -> bb
        _ -> error . compilerError $ "Attempted to access bb index supplied from BFS ordering that does not exist: bbIndex=" ++ (show index)

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
     in FunctionIr 
            (functionIrIdentifier fn)
            (functionIrArgs fn)
            newBlocks
            newCFG

-- adds edge from source->dest to function's CFG
-- * may add loops in the CFG in the event of while statements
addEdgeToCFG :: Int -> Int -> FunctionIr -> FunctionIr
addEdgeToCFG source dest fn =
    let newCFG = addEdge source dest (functionIrCFG fn)
    in FunctionIr 
            (functionIrIdentifier fn)
            (functionIrArgs fn)
            (functionIrBlocks fn) 
            (addEdge source dest (functionIrCFG fn))

appendCommsToBb :: BasicBlockIr -> [CommandIr] -> BasicBlockIr
appendCommsToBb bb comms = BasicBlockIr (bbIrFnName bb) (bbIndex bb) (bbIrPhiFn bb) (comms ++ (bbIrCommands bb))

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
