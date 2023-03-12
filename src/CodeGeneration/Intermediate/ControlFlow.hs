{-# LANGUAGE NamedFieldPuns #-}

module CodeGeneration.Intermediate.ControlFlow where

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
    instr :: Instr a
  } deriving (Show, Eq)

toCFG :: Instrs a -> CFG a
toCFG instrs
  = CFG {
    nodes = M.fromList labelledInstrs,
    edges = findEdges labelledInstrs
  }
  where
    labelledInstrs = zipWith (\n instr -> (assignId instr n, CFGNode{ instr })) [0..] instrs 

findEdges :: [(Id, CFGNode a)] -> Set (Id, Id)
findEdges [] = Set.empty 
findEdges (x:[]) = Set.empty
findEdges ((i, CFGNode {instr = instr}):xs@((i', x'):_)) 
  = case jmpType instr of
      Nothing -> insertNextEdge (findEdges xs)
      Just LinearJmp -> insertBranchEdge (findEdges xs)
      Just CondJmp -> (insertNextEdge . insertBranchEdge) $ findEdges xs
    where
      insertNextEdge = Set.insert (i, i') 
      insertBranchEdge = Set.insert (i, Left $ getLabel instr)
    
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
