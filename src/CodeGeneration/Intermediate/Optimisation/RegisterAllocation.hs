module CodeGeneration.Intermediate.Optimisation.RegisterAllocation (allocRegisters) where

import CodeGeneration.Intermediate.ControlFlow
import CodeGeneration.Intermediate.DataFlow (solveLiveRangesDFE)
import CodeGeneration.Intermediate.IR
import CodeGeneration.Utils (temps)
import Data.Map (Map, (!))
import Data.Maybe
import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as Set

-- Type a denotes the source register type, type b denotes the target register type
type RIG a b = Map a (Set (Either a b))

allTemps :: CFG IRReg -> [IRReg]
allTemps g = Set.toList (foldr ((<>) . temps . instr) Set.empty (M.elems (nodes g)))

buildInterferenceGraph :: CFG IRReg -> RIG IRReg IRReg
buildInterferenceGraph g 
  = M.fromList [(t, interferesWith t)| t <- allTemps g]
  where
    interferesWith :: IRReg -> Set (Either IRReg IRReg)
    interferesWith t 
      = foldr ((<>) . interferesWith' . Set.map Left . liveOuts) Set.empty (nodes g)
      where
        interferesWith' :: Set (Either IRReg IRReg) -> Set (Either IRReg IRReg)
        interferesWith' los
          | Set.member (Left t) los = Set.singleton (Left t) <> los
          | otherwise = los

type Colouring a b = Map a b

colour :: CFG IRReg -> RIG IRReg IRReg -> Colouring IRReg IRReg
colour cfg rig 
  = colour' startingAmount
  where
    startingAmount = 2
    colour' :: Int -> Colouring IRReg IRReg
    colour' i 
      = case findColouring cfg rig (map TmpReg [0..i]) of
          Just colouring -> colouring
          _ -> colour' (i + 1)

findColouring :: CFG IRReg -> RIG IRReg IRReg -> [IRReg] -> Maybe (Colouring IRReg IRReg)
findColouring cfg rig regs = colourings
  where
    colourings = findColouring' (allTemps cfg) rig
    findColouring' :: [IRReg] -> RIG IRReg IRReg -> Maybe (Colouring IRReg IRReg)
    findColouring' [] _ = Just M.empty
    findColouring' (t:ts) rig'
      | null cs = Nothing
      | otherwise = Just $ head cs
      where
        cs = catMaybes [M.insert t r <$> findColouring' ts (M.adjust (Set.insert (Right r)) t rig') | (_, r) <- validMappings]
        validMappings = [(t, r) | (_, r) <- [(t, r) | r <- regs, not $ interferes t r rig']]

interferes :: IRReg -> IRReg -> RIG IRReg IRReg -> Bool
interferes t r rig
  = Set.member (Right r) (rig ! t) 

allocRegisters :: IRInstrs -> IRInstrs
allocRegisters instrs
  = map (replace colouring) instrs
  where
    colouring = colour cfg rig
    cfg = solveLiveRangesDFE $ toCFG instrs
    rig = buildInterferenceGraph cfg

replace :: Colouring IRReg IRReg -> Instr IRReg -> Instr IRReg
replace c (Load op1 op2) = Load (replaceOp c op1) (replaceOp c op2)
replace c (LoadB op1 op2) = LoadB (replaceOp c op1) (replaceOp c op2)
replace c (Store op1 op2) = Store (replaceOp c op1) (replaceOp c op2)
replace c (StoreB op1 op2) = StoreB (replaceOp c op1) (replaceOp c op2)
replace c (Mov op1 op2) = Mov (replaceOp c op1) (replaceOp c op2)
replace c (Add op1 op2 op3) = Add (replaceOp c op1) (replaceOp c op2) (replaceOp c op3)
replace c (Sub op1 op2 op3) = Sub (replaceOp c op1) (replaceOp c op2) (replaceOp c op3)
replace c (Mul op1 op2 op3) = Mul (replaceOp c op1) (replaceOp c op2) (replaceOp c op3)
replace c (Div op1 op2 op3) = Div (replaceOp c op1) (replaceOp c op2) (replaceOp c op3)
replace c (Mod op1 op2 op3) = Mod (replaceOp c op1) (replaceOp c op2) (replaceOp c op3)
replace c (Cmp op1 op2) = Cmp (replaceOp c op1) (replaceOp c op2) 
replace c (Push op1) = Push (replaceOp c op1)
replace c (Pop op1) = Pop (replaceOp c op1)
replace _ i = i

replaceOp :: Colouring IRReg IRReg -> Operand IRReg -> Operand IRReg
replaceOp c (Reg r) = Reg (M.findWithDefault r r c)
replaceOp c (Regs rs) = Regs (map (\r -> M.findWithDefault r r c) rs)
replaceOp c (Ind r) = Ind (M.findWithDefault r r c)
replaceOp c (ImmOffset r i) = ImmOffset (M.findWithDefault r r c) i
replaceOp c (ASR r i) = ASR (M.findWithDefault r r c) i
replaceOp _ (Imm x) = Imm x
replaceOp _ (Abs x) = Abs x