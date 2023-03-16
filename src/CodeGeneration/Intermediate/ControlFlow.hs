{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Intermediate.ControlFlow 
  ( CFG (..)
  , CFGNode (..)  
  , Id
  , toCFG
  , fromCFG
  , mkCFGNode
  , predecessors
  , successors 
  , definers
  , definersBefore
  , users
  )
where

import CodeGeneration.Intermediate.IR
import Data.Map (Map, (!))
import Data.Set (Set)
import Prelude hiding (id)

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T

type Id = Either Label Int

data JmpType = CondJmp | LinearJmp 

data CFG a 
  = CFG {
    nodes :: Map Id (CFGNode a),
    edges :: Set (Id, Id)
  } deriving (Show, Eq)

data CFGNode a
  = CFGNode {
    id :: Id,
    instr :: Instr a,
    isEntry :: Bool,
    isExit :: Bool,
    dominators :: Set Id,
    revDominators :: Set Id,
    liveIns :: Set a,
    liveOuts :: Set a
  } deriving (Show, Eq)

toCFG :: Instrs a -> CFG a
toCFG [] = CFG { nodes = M.empty, edges = Set.empty }
toCFG (entryInstr:instrs)
  = CFG {
    nodes = M.fromList allLabelled,
    edges = findEdges allLabelled
  }
  where
    allLabelled = labelledEntry : labelledInstrs
    labelledInstrs = zipWith (\n instr -> (assignId instr n, mkCFGNode (assignId instr n) instr False)) [1..] instrs 
    labelledEntry = (i0, mkCFGNode i0 entryInstr True)
    i0 = assignId entryInstr 0

mkCFGNode :: Id -> Instr a -> Bool -> CFGNode a
mkCFGNode i instr isEntry 
  = CFGNode { id = i
            , instr = instr
            , isEntry = isEntry
            , isExit = isExit' instr
            , dominators = Set.empty
            , revDominators = Set.empty
            , liveIns = Set.empty
            , liveOuts = Set.empty }
  where
    isExit' :: Instr a -> Bool
    isExit' (Jsr "exit") = True
    isExit' (Define l) = T.isSuffixOf "_return" l
    isExit' (Jmp l) = T.isSuffixOf "_return" l
    isExit' _ = False

fromCFG :: CFG a -> Instrs a
fromCFG = undefined

findEdges :: [(Id, CFGNode a)] -> Set (Id, Id)
findEdges ((i, CFGNode {instr = instr}):xs@((i', _):_)) 
  = case jmpType instr of
      Nothing -> insertNextEdge (findEdges xs)
      Just LinearJmp -> insertBranchEdge (findEdges xs)
      Just CondJmp -> (insertNextEdge . insertBranchEdge) $ findEdges xs
    where
      insertNextEdge = Set.insert (i, i') 
      insertBranchEdge = Set.insert (i, Left $ getLabel instr)
findEdges _ = Set.empty 
    
assignId :: Instr a -> Int -> Id
assignId (Define l) _ = Left l
assignId _ n = Right n

jmpType :: Instr a -> Maybe JmpType
jmpType (Jmp _) = Just LinearJmp
jmpType (Jsr _) = Just LinearJmp
jmpType (JsrOverflow _) = Just LinearJmp
jmpType (JsrNE _) = Just LinearJmp
jmpType (Je _) = Just CondJmp
jmpType (Jne _) = Just CondJmp
jmpType (Jl _) = Just CondJmp
jmpType (Jg _) = Just CondJmp
jmpType (Jle _) = Just CondJmp
jmpType (Jge _) = Just CondJmp
jmpType _ = Nothing

getLabel :: Instr a -> Label
getLabel (Jmp l) = l
getLabel (Jsr l) = l
getLabel (JsrOverflow l) = l
getLabel (JsrNE l) = l
getLabel (Je l) = l
getLabel (Jne l) = l
getLabel (Jl l) = l
getLabel (Jg l) = l
getLabel (Jle l) = l
getLabel (Jge l) = l
getLabel _ = error "can't get the label of a non jump instruction"

successors :: Id -> CFG a -> [Id]
successors i cfg 
  = [dst | (src, dst) <- Set.toList (edges cfg), src == i]

predecessors :: Id -> CFG a -> [Id]
predecessors i cfg 
  = [src | (src, dst) <- Set.toList (edges cfg), dst == i]

-- Returns a list of nodes that define an operand before a given node
definersBefore :: Eq a => Id -> Operand a -> CFG a -> Set Id
definersBefore i op cfg
  = Set.intersection (Set.fromList $ definers op cfg) (Set.fromList $ predecessors i cfg)

-- Returns a list of nodes that define an operand
definers :: Eq a => Operand a -> CFG a -> [Id]
definers op CFG{nodes = ns} 
  = M.keys $ M.filter (\CFGNode{instr = i} -> i `defines` op) ns

defines :: Eq a => Instr a -> Operand a -> Bool
defines (Load dst _) op = op == dst 
defines (LoadB dst _) op = op == dst
defines (Mov dst _) op = op == dst 
defines (Add dst _ _) op = op == dst
defines (Sub dst _ _) op = op == dst
defines (Mul dst _ _) op = op == dst
defines (Div dst _ _) op = op == dst
defines (Mod dst _ _) op = op == dst
defines (Pop dst) op = op == dst 
defines _ _ = False

-- Returns a list of nodes that use an operand
users :: Eq a => Operand a -> CFG a -> [Id]
users op CFG{nodes = ns} 
  = M.keys $ M.filter (\CFGNode{instr = i} -> i `uses` op) ns

uses :: Eq a => Instr a -> Operand a -> Bool
uses (Load _ op1) op = op == op1 
uses (LoadB _ op1) op = op == op1
uses (Store op1 _) op = op == op1 
uses (StoreB op1 _) op = op == op1
uses (Mov _ op1) op = op == op1 
uses (Add _ op1 op2) op = op == op1 || op == op2 
uses (Sub _ op1 op2) op = op == op1 || op == op2
uses (Mul _ op1 op2) op = op == op1 || op == op2
uses (Div _ op1 op2) op = op == op1 || op == op2
uses (Mod _ op1 op2) op = op == op1 || op == op2
uses (Cmp op1 op2) op = op == op1 || op == op2
uses (Push op1) op = op == op1
uses _ _ = False
