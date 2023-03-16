{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.ARM.Peephole (optimise) where 

import CodeGeneration.Intermediate.IR
import CodeGeneration.ARM.Registers
import Data.Maybe

optimise :: Program ArmReg -> Program ArmReg
optimise = map optimiseSection 

optimiseSection :: Section ArmReg -> Section ArmReg
optimiseSection (Section d body) = Section d (optimiseBody body)

optimiseBody :: Body ArmReg -> Body ArmReg
optimiseBody (Body l g instrs) = Body l g ((optimiseInstrs instrs))

optimiseInstrs :: [Instr ArmReg] -> [Instr ArmReg]
optimiseInstrs [] = []
optimiseInstrs ((Add (Reg SP) (Reg SP) (Imm 0)) : instrs) = optimiseInstrs instrs
optimiseInstrs ((Sub (Reg SP) (Reg SP) (Imm 0)) : instrs) = optimiseInstrs instrs
optimiseInstrs (pre : (Push (Regs [r])) : (Pop (Regs [r1])) : instrs) = optimiseInstrs (pre : (Mov (Reg r1) (Reg r)) : instrs)

optimiseInstrs (m@(Mov o1 (Imm _)) : p@(Push _) : instrs) = m : optimiseInstrs (p : instrs)
optimiseInstrs (m@(Mov o1@(Reg r) o2@(Reg r1)) : p@(Push (Regs [r2])) : instrs) 
    | isScratchOperand o1 && r == r2 = optimiseInstrs (Push (Regs [r1]): instrs)
    | otherwise = m : optimiseInstrs (p : instrs)

optimiseInstrs ((Store o1 o2) : (Load o3 o4) : instrs)
    | o2 == o4 && o1 == o3 = optimiseInstrs ((Store o1 o2) : instrs)
    | o2 == o4 = optimiseInstrs ((Store o1 o2) : (Mov o3 o1) : instrs) 
    | otherwise = (Store o1 o2) : (Load o3 o4) : optimiseInstrs instrs
optimiseInstrs (m@(Mov o1 (Imm _)) : s@(Store o3 o4) : instrs) = m : optimiseInstrs (s : instrs)
optimiseInstrs (m@(Mov o1 o2) : s@(Store o3 o4) : instrs)
    | o1 == o3 && isScratchOperand o1 =  optimiseInstrs ((Store o2 o4)  : instrs)
    | otherwise = m : optimiseInstrs (s : instrs)
optimiseInstrs (m1@(Mov o1 o2@(Imm _)) : m2@(Mov o3 o4) : instrs)
    | o1 == o4 && (isScratchOperand o1)= optimiseInstrs ((Mov o3 o2) : instrs)
    | otherwise = m1 : optimiseInstrs (m2 : instrs)
optimiseInstrs (m1@(Mov o1 o2) : m2@(Mov o3 o4) : instrs)
    | o1 == o4 && o2 == o3 && (isScratchOperand o1) && (isScratchOperand o3) =  optimiseInstrs (instrs)
    | o1 == o4 && o2 == o3 =  case optimisedMov1 of 
                                    Nothing -> optimiseInstrs instrs
                                    (Just mov) -> optimiseInstrs (mov : instrs)
    | o1 == o4 && (isScratchOperand o1) && (isScratchOperand o3) = optimiseInstrs ((Mov o3 o2) : instrs)
    | otherwise = case optimisedMov1 of 
                        Nothing -> optimiseInstrs (m2 : instrs)
                        (Just mov) -> mov : optimiseInstrs (m2 : instrs)
    where
        optimisedMov1 = optimiseMov m1

optimiseInstrs (m@(Mov _ _) : instrs) 
    = case (optimiseMov m) of 
            Nothing -> optimiseInstrs instrs
            (Just mov) -> mov : optimiseInstrs instrs
optimiseInstrs (catch : instrs) = catch : optimiseInstrs instrs

optimiseMov :: Instr ArmReg -> Maybe (Instr ArmReg)
optimiseMov (Mov o1 o2)
    = if o1 == o2 then Nothing else Just (Mov o1 o2)
optimiseMov i = Just i 