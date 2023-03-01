module CodeGeneration.ARM.Registers (ArmInstr, ArmInstrs, ArmReg, irToArm) where

import CodeGeneration.IR

import Control.Composition ((.*))
import Control.Monad.State

import qualified Data.Map as M

type ArmInstr = Instr ArmReg
type ArmInstrs = [ArmInstr]

data ArmReg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC
  deriving (Show, Eq)

type FPOffset = Int

data Aux = Aux {
  regsAvailable :: [ArmReg],
  regLocs :: M.Map IRReg (Either ArmReg FPOffset) }

type ArmTranslator a = State Aux a

initAux :: Aux
initAux = Aux {
    regsAvailable = [],
    regLocs = M.empty
  }

retReg :: ArmReg
retReg = R0

irToArm :: Program IRReg -> Program ArmReg
irToArm p = evalState (transProg p) initAux

transProg :: Program IRReg -> ArmTranslator (Program ArmReg)
transProg = mapM transSection

transSection :: Section IRReg -> ArmTranslator (Section ArmReg)
transSection (Section d (Body label global instrs)) 
  = mapM transInstr instrs >>= (\iss -> return $ Section d (Body label global (concat iss)))

transInstr :: Instr IRReg -> ArmTranslator [Instr ArmReg]
transInstr (Load o1 o2) = (:[]) .* Load <$> transOperand o1 <*> transOperand o2
transInstr (Store o1 o2) = (:[]) .* Store <$> transOperand o1 <*> transOperand o2
transInstr (Mov o1 o2) = (:[]) .* Mov <$> transOperand o1 <*> transOperand o2
transInstr (Add o1 o2 o3) = (:[]) .* Add <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Sub o1 o2 o3) = (:[]) .* Sub <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Mul o1 o2 o3) = (:[]) .* Mul <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Div o1 o2 o3) = (:[]) .* Div <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Cmp o1 o2) = (:[]) .* Mov <$> transOperand o1 <*> transOperand o2
transInstr (Jsr l) = return [Jsr l]
transInstr (Push o) = (:[]) . Push <$> transOperand o
transInstr (Pop o) = (:[]) . Pop <$> transOperand o
transInstr (Jmp l) = return [Jmp l]
transInstr (Je l) = return [Je l]
transInstr (Jne l) = return [Jne l]
transInstr (Jl l) = return [Jl l]
transInstr (Jg l) = return [Jg l]
transInstr (Jle l) = return [Jle l]
transInstr (Jge l) = return [Jge l]
transInstr (Define l) = return [Define l]
transInstr (Comment c) = return [Comment c]

transOperand :: Operand IRReg -> ArmTranslator (Operand ArmReg)
transOperand (Reg r) = Reg <$> transIRReg r
transOperand (Regs rs) = Regs <$> mapM transIRReg rs
transOperand (Ind r) = Ind <$> transIRReg r
transOperand (ImmOffset r offset) = flip ImmOffset offset <$> transIRReg r
transOperand (Imm i) = return $ Imm i
transOperand (Abs l) = return $ Abs l

transIRReg :: IRReg -> ArmTranslator ArmReg
transIRReg (TmpReg n) = return R0
transIRReg (IRParam n) = return R0
transIRReg IRFP = return FP
transIRReg IRSP = return SP
transIRReg IRLR = return LR
transIRReg IRPC = return PC
transIRReg IRRet = return retReg