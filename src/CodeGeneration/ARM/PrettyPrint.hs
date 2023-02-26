{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.ARM.PrettyPrint (showArm) where

import CodeGeneration.ARM.Registers
import CodeGeneration.IR
import qualified Data.Text as T

showArm :: Program ArmReg -> T.Text
showArm = T.intercalate "\n" . map showSection

showSection :: Section ArmReg -> T.Text
showSection (Section ds f) =
  T.intercalate "\n" [".data", showData ds, ".text", showFunction f, ""]

showData :: [Data] -> T.Text
showData = T.intercalate "\n" . map (\(StringData l v) -> l <> ":\n\t.asciz \"" <> v <> "\"")

showFunction :: Body ArmReg -> T.Text
showFunction (Body l g instrs) =
  T.intercalate
    "\n"
    ( [".global " <> l | g]
        ++ [l <> ":"]
        ++ map (("\t" <>) . T.unwords . showInstr) instrs
    )

showInstr :: ArmInstr -> [T.Text]
showInstr (Load rd a) = ["ldr", showOps [rd, a]]
showInstr (Store rd a) = ["str", showOps [rd, a]]
showInstr (Mov rd o2) = ["mov", showOps [rd, o2]]
showInstr (Add rd rn o2) = ["add", showOps [rd, rn, o2]]
showInstr (Sub rd rn o2) = ["sub", showOps [rd, rn, o2]]
showInstr (Mul rd rm rs) = ["mul", showOps [rd, rm, rs]]
showInstr (Div {}) = ["@ div"]
showInstr (Cmp rn o2) = ["cmp", showOps [rn, o2]]
showInstr (Jmp l) = ["b", l]
showInstr (Jsr l) = ["bl", l]
showInstr (Je l) = ["beq", l]
showInstr (Jl l) = ["blt", l]
showInstr (Jg l) = ["bgt", l]
showInstr (Jle l) = ["ble", l]
showInstr (Jge l) = ["bge", l]
showInstr (Push o) = ["push", showOp o]
showInstr (Pop o) = ["pop", showOp o]
showInstr (Comment t) = ["@", t]

showOps :: [Operand ArmReg] -> T.Text
showOps = T.intercalate ", " . map showOp

showOp :: Operand ArmReg -> T.Text
showOp (Reg r) = showArmReg r
showOp (Regs rs) = "{" <> T.intercalate ", " (map showArmReg rs) <> "}"
showOp (Imm i) = "#" <> T.pack (show i)
showOp (Abs i) = "=" <> i
showOp (Ind r) = "[" <> showOp (Reg r) <> "]"
showOp (ImmOffset r i) = "[" <> showOp (Reg r) <> ", " <> showOp (Imm i) <> "]"

showArmReg :: ArmReg -> T.Text
showArmReg = T.toLower . T.pack . show
