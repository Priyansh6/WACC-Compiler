module CodeGeneration.Intermediate.ControlFlow 
  ( CFG (..)
  , CFGNode (..)  
  , toCFG
  , mkCFGNode
  , predecessors
  , successors 
  )
where

import CodeGeneration.Intermediate.IR
import Data.Map (Map, (!))
import Data.Set (Set)

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
    instr :: Instr a,
    dominators :: Set Id
  } deriving (Show, Eq)

toCFG :: Instrs a -> CFG a
toCFG instrs
  = CFG {
    nodes = M.fromList labelledInstrs,
    edges = findEdges labelledInstrs
  }
  where
    labelledInstrs = zipWith (\n instr -> (assignId instr n, mkCFGNode instr)) [0..] instrs 

mkCFGNode :: Instr a -> CFGNode a
mkCFGNode instr 
  = CFGNode { instr = instr, dominators = Set.empty }

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

successors :: Id -> CFG a -> [CFGNode a]
successors id cfg 
  = map ((nodes cfg) !) succIds
  where
    succIds = [dst | (src, dst) <- Set.toList (edges cfg), src == id]

predecessors :: Id -> CFG a -> [CFGNode a]
predecessors id cfg 
  = map ((nodes cfg) !) predIds
  where
    predIds = [src | (src, dst) <- Set.toList (edges cfg), dst == id]
