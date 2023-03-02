{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.ARM.Registers (ArmInstr, ArmInstrs, ArmReg (..), transProg) where

import CodeGeneration.Helpers (HelperFunc (..), showHelperLabel)
import CodeGeneration.IR

import Control.Monad.State
import Control.Monad.Writer
import Data.Map ((!))

import qualified Data.Map as M
import qualified Data.Set as S

type ArmInstr = Instr ArmReg
type ArmInstrs = [ArmInstr]

data ArmReg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | R12 | SP | LR | PC
  deriving (Show, Eq, Ord)
type ArmRegs = S.Set ArmReg

type FPOffset = Int

data Aux = Aux {
  nextFPOffset :: FPOffset,
  scratchesAvailable :: ArmRegs,
  regsAvailable :: ArmRegs,
  regLocs :: M.Map IRReg (Either ArmReg FPOffset) }

type ArmTranslator a = State Aux a
type ArmMemoryAllocator a = WriterT (ArmInstrs, ArmInstrs, ArmRegs) (State Aux) a

initAux :: Aux
initAux = Aux {
    nextFPOffset = -4,
    scratchesAvailable = scratchRegs,
    regsAvailable = generalRegs,
    regLocs = M.empty
  }

generalRegs :: ArmRegs
generalRegs = S.fromList [R4, R5, R6, R7]

paramRegs :: ArmRegs
paramRegs = S.fromList [R0, R1, R2, R3]

scratchRegs :: ArmRegs
scratchRegs = S.fromList [R8, R10, R12]

retReg :: ArmReg
retReg = R0

divModLabel :: Label
divModLabel = "__aeabi_idivmod"

transProg :: Program IRReg -> Program ArmReg
transProg = map (flip evalState initAux . transSection)

transSection :: Section IRReg -> ArmTranslator (Section ArmReg)
transSection (Section d (Body label global instrs)) = do
  section <- concat <$> mapM transAndAddMemoryInstrs instrs
  fpOff <- gets nextFPOffset
  return (Section d (Body label global ([Push (Regs [FP, LR]), Mov (Reg FP) (Reg SP), Add (Reg SP) (Reg SP) (Imm fpOff)] ++ section ++ [Sub (Reg SP) (Reg SP) (Imm fpOff), Pop (Regs [FP, PC])])))

transAndAddMemoryInstrs :: Instr IRReg -> ArmTranslator ArmInstrs
transAndAddMemoryInstrs instr = do
  (translatedInstr, (prefixInstrs, suffixInstrs, rs)) <- runWriterT (transInstr instr)
  _ <- mapM makeScratchAvailable $ S.toList rs
  let translatedInstrs = transDivMod translatedInstr
  return $ prefixInstrs ++ translatedInstrs ++ suffixInstrs
  where
    transDivMod :: ArmInstr -> ArmInstrs
    transDivMod (Div o1 o2 o3)
      = [
        Mov (Reg R0) o2, 
        Mov (Reg R1) o3, 
        Cmp (Reg R1) (Imm 0), 
        Jle (showHelperLabel ErrDivZero),
        Jsr divModLabel,
        Mov o1 (Reg R0)
      ]
    transDivMod (Mod o1 o2 o3)
      = [
        Mov (Reg R0) o2, 
        Mov (Reg R1) o3, 
        Cmp (Reg R1) (Imm 0), 
        Jle (showHelperLabel ErrDivZero),
        Jsr divModLabel,
        Mov o1 (Reg R1)
      ]
    transDivMod i = [i]

transInstr :: Instr IRReg -> ArmMemoryAllocator ArmInstr
transInstr (Load o1 o2) = Load <$> transOperand o1 True <*> transOperand o2 False
transInstr (Store o1 o2) = Store <$> transOperand o1 False <*> transOperand o2 True
transInstr (Mov o1 o2) = Mov <$> transOperand o1 True <*> transOperand o2 False 
transInstr (Add o1 o2 o3) = Add <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False 
transInstr (Sub o1 o2 o3) = Sub <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False 
transInstr (Mul o1 o2 o3) = Mul <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False 
transInstr (Div o1 o2 o3) = Div <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False
transInstr (Mod o1 o2 o3) = Mod <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False
transInstr (Cmp o1 o2) = Cmp <$> transOperand o1 False <*> transOperand o2 False
transInstr (Jsr l) = return $ Jsr l
transInstr (Push o) = Push <$> transOperand o False
transInstr (Pop o) = Pop <$> transOperand o True
transInstr (Jmp l) = return $ Jmp l
transInstr (Je l) = return $ Je l
transInstr (Jne l) = return $ Jne l
transInstr (Jl l) = return $ Jl l
transInstr (Jg l) = return $ Jg l
transInstr (Jle l) = return $ Jle l
transInstr (Jge l) = return $ Jge l
transInstr (Define l) = return $ Define l
transInstr (Comment c) = return $ Comment c

transOperand :: Operand IRReg -> Bool -> ArmMemoryAllocator (Operand ArmReg)
transOperand (Reg r) isDst = Reg <$> transIRReg r isDst
transOperand (Regs rs) isDst = Regs <$> mapM (flip transIRReg isDst) rs
transOperand (Ind r) _ = Ind <$> transIRReg r False
transOperand (ImmOffset r offset) _ = flip ImmOffset offset <$> transIRReg r False
transOperand (Imm i) _ = return $ Imm i
transOperand (Abs l) _ = return $ Abs l

transIRReg :: IRReg -> Bool -> ArmMemoryAllocator ArmReg
transIRReg tr@(TmpReg _) isDst = allocateRegLoc tr generalRegs >> transDynamicReg tr isDst
-- transIRReg pr@(IRParam _) isDst = allocateRegLoc pr paramRegs >> transDynamicReg pr isDst
transIRReg pr@(IRParam _) isDst = allocateParamRegLoc pr >> transDynamicReg pr isDst
transIRReg IRScratch1 _ = return $ S.elemAt 0 scratchRegs
transIRReg IRScratch2 _ = return $ S.elemAt 1 scratchRegs
transIRReg IRScratch3 _ = return $ S.elemAt 2 scratchRegs
transIRReg IRFP _ = return FP
transIRReg IRSP _ = return SP
transIRReg IRLR _ = return LR 
transIRReg IRPC _ = return PC
transIRReg IRRet _ = return retReg

allocateParamRegLoc :: IRReg -> ArmMemoryAllocator ()
allocateParamRegLoc r@(IRParam n)
  | n < length paramRegs = modify (\a@Aux {regLocs = locs} -> a {regLocs = M.insert r (Left (paramRegMappings ! r)) locs}) 
  | otherwise = modify (\a@Aux {regLocs = locs, nextFPOffset = fpOff} -> a {regLocs = M.insert r (Right fpOff) locs, nextFPOffset = fpOff - 4})
  where
    paramRegMappings = M.fromList [(IRParam 0, R0), (IRParam 1, R1), (IRParam 2, R2), (IRParam 3, R3)]
allocateParamRegLoc _ = undefined

transDynamicReg :: IRReg -> Bool -> ArmMemoryAllocator ArmReg
transDynamicReg r isDst = do
  locs <- gets regLocs
  case (M.lookup r locs) of
    Nothing -> error("Can't translate register if it hasn't been allocated a location.")
    Just (Left armReg) -> return armReg
    Just (Right fpOff) -> do
      sr <- getFreeScratch
      tell ([],[], S.fromList [sr])
      tell $ if isDst
        then ([], [Store (Reg sr) (ImmOffset FP fpOff)], S.empty)
        else ([Load (Reg sr) (ImmOffset FP fpOff)], [], S.empty)
      return sr

allocateRegLoc :: IRReg -> ArmRegs -> ArmMemoryAllocator ()
allocateRegLoc r regOptions = do
  locs <- gets regLocs
  case (M.lookup r locs) of
    Nothing -> do
      loc <- getFreeLoc regOptions
      modify (\a -> a {regLocs = M.insert r loc locs})
    _ -> return ()

getFreeLoc :: ArmRegs -> ArmMemoryAllocator (Either ArmReg FPOffset)
getFreeLoc regOptions = do
  rs <- gets regsAvailable
  let nextRs = regOptions `S.intersection` rs
  if null nextRs
    then do
      fpOff <- gets nextFPOffset
      modify (\a -> a {nextFPOffset = fpOff - 4})
      return $ Right fpOff
    else do
      let nextR = S.elemAt 0 nextRs
      modify (\a -> a {regsAvailable = S.delete nextR rs})
      return $ Left nextR

getFreeScratch :: ArmMemoryAllocator ArmReg
getFreeScratch = state (\a@Aux {scratchesAvailable = rs} -> let r = S.elemAt 0 rs in (r, a {scratchesAvailable = S.delete r rs}))

makeScratchAvailable :: ArmReg -> ArmTranslator ()
makeScratchAvailable r = modify (\a@Aux {scratchesAvailable = rs} -> a {scratchesAvailable = S.insert r rs})