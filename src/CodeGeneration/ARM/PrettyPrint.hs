{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.ARM.PrettyPrint (showArm) where

import CodeGeneration.ARM.Registers (ArmInstr, ArmReg, overflowReg)
import CodeGeneration.Intermediate.IR
import qualified Data.Text as T

showArm :: Program ArmReg -> T.Text
showArm = T.intercalate "\n" . map showSection

showSection :: Section ArmReg -> T.Text
showSection (Section ds body) =
  T.unlines $ showData ds ++ [showBody body]

showData :: [Data] -> [T.Text]
showData ds =
  [".data" | not (null ds)]
    ++ map (\(StringData l v) -> "\t.word " <> T.pack (show (T.length v)) <> "\n" <> l <> ":\n\t.asciz " <> T.pack (show v)) ds

showBody :: Body ArmReg -> T.Text
showBody (Body l g instrs) =
  T.intercalate "\n" $
    ".text"
      : [".global " <> l | g]
      ++ [l <> ":"]
      ++ map showIndentedInstr instrs

showIndentedInstr :: ArmInstr -> T.Text
showIndentedInstr i@(Define _) = showInstr i
showIndentedInstr i = "\t" <> showInstr i

showInstr :: ArmInstr -> T.Text
showInstr (Load rd a) = "ldr " <> showOps [rd, a]
showInstr (Store rd a) = "str " <> showOps [rd, a]
showInstr (LoadB rd a) = "ldrb " <> showOps [rd, a]
showInstr (StoreB rd a) = "strb " <> showOps [rd, a]
showInstr (Mov rd o2) = "mov " <> showOps [rd, o2]
showInstr (Add rd rn o2) = "adds " <> showOps [rd, rn, o2]
showInstr (Sub rd rn o2) = "subs " <> showOps [rd, rn, o2]
showInstr (Mul rd rm rs) = "smull " <> showOps [rd, Reg overflowReg, rm, rs]
showInstr (Div {}) = error "div instruction not supported in ARM"
showInstr (Mod {}) = error "mod instruction not supported in ARM"
showInstr (Cmp rn o2) = "cmp " <> showOps [rn, o2]
showInstr (Jsr l) = "bl " <> l
showInstr (JsrOverflow l) = "blvs " <> l
showInstr (JsrNE l) = "blne " <> l
showInstr (Jmp l) = "b " <> l
showInstr (Je l) = "beq " <> l
showInstr (Jne l) = "bne " <> l
showInstr (Jl l) = "blt " <> l
showInstr (Jg l) = "bgt " <> l
showInstr (Jle l) = "ble " <> l
showInstr (Jge l) = "bge " <> l
showInstr (Push o) = "push " <> "{" <> showOp o <> "}"
showInstr (Pop o) = "pop " <> "{" <> showOp o <> "}"
showInstr (Comment t) = "@ " <> t
showInstr (Define l) = l <> ":"

showOps :: [Operand ArmReg] -> T.Text
showOps = T.intercalate ", " . map showOp

showOp :: Operand ArmReg -> T.Text
showOp (Reg r) = T.toLower (T.pack (show r))
showOp (Regs []) = error "Expected a non-empty list of registers"
showOp (Regs rs) = T.intercalate ", " (map (showOp . Reg) rs)
showOp (Imm i) = "#" <> T.pack (show i)
showOp (Abs i) = "=" <> i
showOp (Ind r) = "[" <> showOp (Reg r) <> "]"
showOp (ImmOffset r i) = "[" <> showOp (Reg r) <> ", " <> showOp (Imm i) <> "]"
showOp (ASR r i) = showOp (Reg r) <> ", asr " <> showOp (Imm i)
