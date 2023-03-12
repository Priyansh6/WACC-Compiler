module CodeGeneration.Intermediate.DataFlow where

import CodeGeneration.Intermediate.ControlFlow 
import CodeGeneration.Intermediate.IR
import Data.List (foldl', foldl1')
import Data.Map ((!))
import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as Set

type Id = Either Label Int

-- Updates the dominators list for a given CFG node
dominatorDFE :: Id -> CFG a -> CFG a
dominatorDFE i cfg@CFG {nodes = nm}
  = cfg {nodes = M.insert i n' nm }
  where
    n' = if null predDominators 
          then n {dominators = (Set.singleton i)} 
          else n {dominators = (Set.singleton i) <> (foldl1' Set.intersection predDominators)} 
    n = (nodes cfg) ! i
    predDominators = map dominators (predecessors i cfg)

initialiseDominatorDFE :: CFG a -> CFG a
initialiseDominatorDFE cfg@CFG {nodes = nm}
  = cfg {nodes = M.mapWithKey initialiseNode nm}
  where
    initialiseNode :: Id -> CFGNode a -> CFGNode a
    initialiseNode i cfgn
      | isEntry cfgn = cfgn {dominators = Set.singleton i}
      | otherwise = cfgn {dominators = Set.fromList (M.keys nm)}

iterateDominatorDFE :: CFG a -> CFG a
iterateDominatorDFE cfg
  = foldl' (flip dominatorDFE) cfg nodeIds
  where
    nodeIds = M.keys $ nodes cfg

solveDominatorDFE :: Eq a => CFG a -> CFG a
solveDominatorDFE cfg
  = solveDominatorDFE' start
  where
    start = initialiseDominatorDFE cfg
    allDominators :: CFG a -> [(Set Id)]
    allDominators = (map dominators) . M.elems . nodes
    solveDominatorDFE' :: Eq a => CFG a -> CFG a
    solveDominatorDFE' g
      | allDominators g == allDominators g' = g
      | otherwise = solveDominatorDFE' g'
      where
        g' = iterateDominatorDFE g

