module CodeGeneration.ARM.Registers 
  ( allocRegisters, 
    ArmInstr,
    ArmInstrs
  ) 
where

import CodeGeneration.IR (Instr, Instrs, IRReg)

data ArmReg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC
type ArmInstr = Instr ArmReg
type ArmInstrs = Instrs ArmReg

-- could be a TRICKY job
allocRegisters :: Instrs IRReg -> Instrs ArmReg
allocRegisters = undefined