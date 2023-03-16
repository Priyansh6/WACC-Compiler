{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.ARM.Registers (isParamOperand, isGeneralOperand, isScratchOperand, ArmInstr, ArmInstrs, ArmReg (..), transProg, overflowReg) where
-- Convert IR to ARM / Allocate ARM registers from an intermediate respresentation --

import AST (WType (WInt))
import CodeGeneration.Intermediate.Helpers (HelperFunc (..), showHelperLabel)
import CodeGeneration.Intermediate.IR
import CodeGeneration.Utils (heapTypeSize)
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

data Aux = Aux
  { nextFPOffset :: FPOffset,
    scratchesAvailable :: ArmRegs,
    regsAvailable :: ArmRegs,
    regLocs :: M.Map IRReg (Either ArmReg FPOffset)
  }

type ArmTranslator a = State Aux a

type ArmMemoryAllocator a = WriterT (ArmInstrs, ArmInstrs, ArmRegs) (State Aux) a

initAux :: Aux
initAux =
  Aux
    { nextFPOffset = -4,
      scratchesAvailable = scratchRegs,
      regsAvailable = generalRegs,
      regLocs = M.empty
    }

allRegs, generalRegs, paramRegs, scratchRegs :: ArmRegs
allRegs = S.fromList [R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, FP, R12, SP, LR, PC]
generalRegs = S.fromList [R4, R5, R6, R7]
paramRegs = S.fromList [R0, R1, R2, R3]
scratchRegs = S.fromList [R8, R10, R12]

isScratchOperand :: Operand ArmReg -> Bool
isScratchOperand (Reg reg) = S.member reg scratchRegs
isScratchOperand _ = False

isGeneralOperand :: Operand ArmReg -> Bool 
isGeneralOperand (Reg reg) = S.member reg generalRegs
isGeneralOperand _ = False

isParamOperand :: Operand ArmReg -> Bool
isParamOperand (Reg reg) = S.member reg paramRegs
isParamOperand _ = False

retReg, overflowReg :: ArmReg
retReg = R0
overflowReg = R9

divModLabel :: Label
divModLabel = "__aeabi_idivmod"

transProg :: Program IRReg -> Program ArmReg
transProg = map (flip evalState initAux . transSection)

transSection :: Section IRReg -> ArmTranslator (Section ArmReg)
transSection (Section d (Body label global instrs)) = do
  section <- concat <$> mapM transAndAddMemoryInstrs instrs
  fpOff <- gets nextFPOffset
  return $
    Section
      d
      ( Body
          label
          global
          ( [ Push (Regs [FP, LR]),
              Push (Regs (S.toList generalRegs)),
              Mov (Reg FP) (Reg SP),
              Add (Reg SP) (Reg SP) (Imm (fpOff + 4))
            ]
              ++ section
              ++ [ Sub (Reg SP) (Reg SP) (Imm (fpOff + 4)),
                   Pop (Regs (S.toList generalRegs)),
                   Pop (Regs [FP, PC])
                 ]
          )
      )

transAndAddMemoryInstrs :: Instr IRReg -> ArmTranslator ArmInstrs
transAndAddMemoryInstrs instr = do
  (translatedInstr, (prefixInstrs, suffixInstrs, rs)) <- runWriterT (transInstr instr)
  mapM_ makeScratchAvailable $ S.toList rs
  let translatedInstrs = transHandleErrors translatedInstr
  return $ prefixInstrs ++ translatedInstrs ++ suffixInstrs
  where
    transHandleErrors :: ArmInstr -> ArmInstrs
    transHandleErrors (Div o1 o2 o3) =
      [ Mov (Reg R0) o2,
        Mov (Reg R1) o3,
        Cmp (Reg R1) (Imm 0),
        Je (showHelperLabel ErrDivZero),
        Jsr divModLabel,
        Mov o1 (Reg R0)
      ]
    transHandleErrors (Mod o1 o2 o3) =
      [ Mov (Reg R0) o2,
        Mov (Reg R1) o3,
        Cmp (Reg R1) (Imm 0),
        Je (showHelperLabel ErrDivZero),
        Jsr divModLabel,
        Mov o1 (Reg R1)
      ]
    transHandleErrors m@(Mul (Reg o1) _ _) =
      [ m,
        Cmp (Reg overflowReg) (ASR o1 (8 * heapTypeSize WInt - 1)),
        JsrNE $ showHelperLabel ErrOverflow
      ]
    transHandleErrors s@(Sub {}) = s : [JsrOverflow $ showHelperLabel ErrOverflow]
    transHandleErrors s@(Add {}) = s : [JsrOverflow $ showHelperLabel ErrOverflow]
    transHandleErrors i = [i]

transInstr :: Instr IRReg -> ArmMemoryAllocator ArmInstr
transInstr (Load o1 o2) = Load <$> transOperand o1 True <*> transOperand o2 False
transInstr (Store o1 o2) = Store <$> transOperand o1 False <*> transOperand o2 True
transInstr (LoadB o1 o2) = LoadB <$> transOperand o1 True <*> transOperand o2 False
transInstr (StoreB o1 o2) = StoreB <$> transOperand o1 False <*> transOperand o2 True
transInstr (Mov o1 o2) = Mov <$> transOperand o1 True <*> transOperand o2 False
transInstr (Add o1 o2 o3) = Add <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False
transInstr (Sub o1 o2 o3) = Sub <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False
transInstr (Mul o1 o2 o3) = Mul <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False
transInstr (Div o1 o2 o3) = Div <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False
transInstr (Mod o1 o2 o3) = Mod <$> transOperand o1 True <*> transOperand o2 False <*> transOperand o3 False
transInstr (Cmp o1 o2) = Cmp <$> transOperand o1 False <*> transOperand o2 False
transInstr (Jsr l) = return $ Jsr l
transInstr (JsrOverflow l) = return $ JsrOverflow l
transInstr (JsrNE l) = return $ JsrNE l
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
transOperand (Regs rs) isDst = Regs . filter (`S.member` allRegs) <$> mapM (flip transIRReg isDst) rs
transOperand (Ind r) _ = Ind <$> transIRReg r False
transOperand (ImmOffset r offset) _ = flip ImmOffset offset <$> transIRReg r False
transOperand (Imm i) _ = return $ Imm i
transOperand (Abs l) _ = return $ Abs l
transOperand (ASR r offset) _ = transIRReg r False >>= (\r' -> return (ASR r' offset))

transIRReg :: IRReg -> Bool -> ArmMemoryAllocator ArmReg
transIRReg tr@(TmpReg _) isDst = allocateRegLoc tr generalRegs >> transDynamicReg tr isDst
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
  case M.lookup r locs of
    Nothing -> error "Can't translate register if it hasn't been allocated a location."
    Just (Left armReg) -> return armReg
    Just (Right fpOff) -> do
      sr <- getFreeScratch
      tell ([], [], S.fromList [sr])
      tell $
        if isDst
          then ([], [Store (Reg sr) (ImmOffset FP fpOff)], S.empty)
          else ([Load (Reg sr) (ImmOffset FP fpOff)], [], S.empty)
      return sr

allocateRegLoc :: IRReg -> ArmRegs -> ArmMemoryAllocator ()
allocateRegLoc r regOptions = do
  locs <- gets regLocs
  case M.lookup r locs of
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