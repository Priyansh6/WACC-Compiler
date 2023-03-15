module CodeGeneration.Intermediate.DataFlow (solveDominatorDFE, solveRevDominatorDFE) where

import CodeGeneration.Intermediate.ControlFlow 
import CodeGeneration.Intermediate.IR
import Data.List (foldl', foldl1')
import Data.Map ((!))
import Data.Set (Set)
import Prelude hiding (id)

import qualified Data.Map as M
import qualified Data.Set as Set

type DFE a = CFG a -> Id -> CFG a
type DFEInitialiser a = CFG a -> CFG a
type DFEIterator a = CFG a -> Id -> CFG a
type Diff a = CFG a -> [Set Id]

iterateDFE :: DFEIterator a -> CFG a -> CFG a
iterateDFE dfe cfg
  = foldl' dfe cfg nodeIds
  where
    nodeIds = M.keys $ nodes cfg

solveDFE :: DFEInitialiser a -> DFE a -> Diff a -> CFG a -> CFG a
solveDFE initialiser dfe diff cfg
  = solveDFE' start
  where
    start = initialiser cfg
    solveDFE' g
      | diff g == diff g' = g
      | otherwise = solveDFE' g'
      where
        g' = iterateDFE dfe g

solveDominatorDFE :: CFG a -> CFG a
solveDominatorDFE
  = solveDFE initialise dfe diff
  where
    initialise cfg@CFG {nodes = nm}
      = cfg {nodes = M.map initialiseNode nm}
      where
        initialiseNode cfgn@CFGNode{id = i}
          | isEntry cfgn = cfgn {dominators = Set.singleton i}
          | otherwise = cfgn {dominators = Set.fromList (M.keys nm)}

    dfe cfg@CFG {nodes = nm} i
      = cfg {nodes = M.insert i n' nm }
      where
        n' = if null predDominators 
              then n {dominators = Set.singleton i} 
              else n {dominators = Set.singleton i <> foldl1' Set.intersection predDominators} 
        n = nodes cfg ! i
        predDominators = map (\i' -> dominators (nm ! i')) (predecessors i cfg)

    diff = map dominators . M.elems . nodes

solveRevDominatorDFE :: CFG a -> CFG a 
solveRevDominatorDFE
  = solveDFE initialise dfe diff
  where
    initialise cfg@CFG {nodes = nm}
      = cfg {nodes = M.map initialiseNode nm}
      where
        initialiseNode cfgn@CFGNode{id = i}
          | isExit cfgn = cfgn {revDominators = Set.singleton i}
          | otherwise = cfgn {revDominators = Set.fromList (M.keys nm)}
    dfe cfg@CFG {nodes = nm} i
      = cfg {nodes = M.insert i n' nm }
      where
        n' = if null succDominators 
              then n {revDominators = Set.singleton i} 
              else n {revDominators = Set.singleton i <> foldl1' Set.intersection succDominators} 
        n = nodes cfg ! i
        succDominators = map (\i' -> revDominators (nm ! i')) (successors i cfg)
    diff = map revDominators . M.elems . nodes
