module CodeGeneration.ARM.Registers (ArmInstr, ArmInstrs, ArmReg, transProg) where

import CodeGeneration.IR

import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as M
import qualified Data.Set as S

type ArmInstr = Instr ArmReg
type ArmInstrs = [ArmInstr]

data ArmReg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC
  deriving (Show, Eq, Ord)
type ArmRegs = S.Set ArmReg

type FPOffset = Int

data Aux = Aux {
  currFPOffset :: FPOffset,
  regsAvailable :: ArmRegs,
  regLocs :: M.Map IRReg (Either ArmReg FPOffset) }

type ArmTranslator a = State Aux a
type ArmMemoryAllocator a = WriterT (ArmInstrs, ArmInstrs) (State Aux) a

initAux :: Aux
initAux = Aux {
    currFPOffset = 0,
    regsAvailable = generalRegs,
    regLocs = M.empty
  }

generalRegs :: ArmRegs
generalRegs = S.fromList [R0, R1, R2, R3, R4, R5, R6, R7]

paramRegs :: ArmRegs
paramRegs = S.fromList [R0, R1, R2, R3]

retReg :: ArmReg
retReg = R0

scratch1 :: ArmReg 
scratch1 = R8

scratch2 :: ArmReg 
scratch2 = R10

scratch3 :: ArmReg 
scratch3 = R12

regsInUse :: ArmRegs -> ArmRegs
regsInUse regsAvailable = generalRegs S.\\ regsAvailable

transProg :: Program IRReg -> Program ArmReg
transProg = map (flip evalState initAux . transSection)

transSection :: Section IRReg -> ArmTranslator (Section ArmReg)
transSection (Section d (Body label global instrs)) 
  = Section d . Body label global . concat <$> mapM transAndAddMemoryInstrs instrs

transAndAddMemoryInstrs :: Instr IRReg -> ArmTranslator ArmInstrs
transAndAddMemoryInstrs instr = do
  (translatedInstr, (loadInstrs, storeInstrs)) <- runWriterT (transInstr instr)
  return $ loadInstrs ++ [translatedInstr] ++ storeInstrs

transInstr :: Instr IRReg -> ArmMemoryAllocator ArmInstr
transInstr (Load o1 o2) = Load <$> transOperand o1 <*> transOperand o2
transInstr (Store o1 o2) = Store <$> transOperand o1 <*> transOperand o2
transInstr (Mov o1 o2) = Mov <$> transOperand o1 <*> transOperand o2
transInstr (Add o1 o2 o3) = Add <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Sub o1 o2 o3) = Sub <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Mul o1 o2 o3) = Mul <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Div o1 o2 o3) = Div <$> transOperand o1 <*> transOperand o2 <*> transOperand o3
transInstr (Cmp o1 o2) = Mov <$> transOperand o1 <*> transOperand o2
transInstr (Jsr l) = return $ Jsr l
transInstr (Push o) = Push <$> transOperand o
transInstr (Pop o) = Pop <$> transOperand o
transInstr (Jmp l) = return $ Jmp l
transInstr (Je l) = return $ Je l
transInstr (Jne l) = return $ Jne l
transInstr (Jl l) = return $ Jl l
transInstr (Jg l) = return $ Jg l
transInstr (Jle l) = return $ Jle l
transInstr (Jge l) = return $ Jge l
transInstr (Define l) = return $ Define l
transInstr (Comment c) = return $ Comment c

transOperand :: Operand IRReg -> ArmMemoryAllocator (Operand ArmReg)
transOperand (Reg r) = Reg <$> transIRReg r
transOperand (Regs rs) = Regs <$> mapM transIRReg rs
transOperand (Ind r) = Ind <$> transIRReg r
transOperand (ImmOffset r offset) = flip ImmOffset offset <$> transIRReg r
transOperand (Imm i) = return $ Imm i
transOperand (Abs l) = return $ Abs l

transIRReg :: IRReg -> ArmMemoryAllocator ArmReg
transIRReg tr@(TmpReg _) = return R0
transIRReg pr@(IRParam _) = return R0
transIRReg IRScratch1 = return scratch1
transIRReg IRScratch2 = return scratch2
transIRReg IRScratch3 = return scratch3
transIRReg IRFP = return FP
transIRReg IRSP = return SP
transIRReg IRLR = return LR
transIRReg IRPC = return PC
transIRReg IRRet = return retReg