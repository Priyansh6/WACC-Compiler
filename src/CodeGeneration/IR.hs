{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.IR (module CodeGeneration.IR) where

import qualified Data.Map as M
import qualified Data.Text as T

type Program a = [Section a]

-- data Section a = Section [Data] [Function a] deriving (Show, Eq)
data Section a = Section [Data] (Body a) deriving (Show, Eq)

data Body a = Body Label Global [Instr a] deriving (Show, Eq)

type Global = Bool

data Data = StringData Label T.Text deriving (Show, Eq)

data IRReg = TmpReg Int | IRParam Int | IRScratch1 | IRScratch2 | IRScratch3 | IRFP | IRSP | IRLR | IRPC | IRRet deriving (Show, Eq, Ord)

type Instrs a = [Instr a]
type IRInstrs = Instrs IRReg

data Ident = Ident T.Text deriving (Show, Ord, Eq)

data Instr a
  = Load (Operand a) (Operand a)
  | Store (Operand a) (Operand a)
  | Mov (Operand a) (Operand a)
  | Add (Operand a) (Operand a) (Operand a)
  | Sub (Operand a) (Operand a) (Operand a)
  | Mul (Operand a) (Operand a) (Operand a)
  | Div (Operand a) (Operand a) (Operand a)
  | Cmp (Operand a) (Operand a)
  | Jmp Label -- Jump to generic label
  | Jsr Label -- Jump to subroutine (updates LR)
  | Je Label
  | Jne Label
  | Jl Label
  | Jg Label
  | Jle Label
  | Jge Label
  | Push (Operand a)
  | Pop (Operand a)
  | Define Label
  | Comment T.Text -- Creates a comment in the assembly file
  deriving (Show, Eq)

type Label = T.Text

data Operand a
  = Reg a
  | Regs [a] -- for Push and Pop
  | Imm Int
  | Abs Label
  | Ind a -- register indirect
  | ImmOffset a Int -- for addressing mode 2
  deriving (Show, Eq)

type FPOffsets = M.Map Ident Int


showIR :: Program IRReg -> T.Text
showIR = T.intercalate "\n" . map showSection

showSection :: Section IRReg -> T.Text
showSection (Section ds body) =
  T.unlines $ showData ds ++ [showBody body]

showData :: [Data] -> [T.Text]
showData ds =
  [".data" | not (null ds)]
    ++ map (\(StringData l v) -> "\t.word " <> T.pack (show (T.length v)) <> "\n" <> l <> ":\n\t.asciz " <> T.pack (show v)) ds

showBody :: Body IRReg -> T.Text
showBody (Body l g instrs) =
  T.intercalate "\n" $
    ".text"
      : [".global " <> l | g]
      ++ [l <> ":"]
      ++ map showIndentedInstr instrs

showIndentedInstr :: Instr IRReg -> T.Text
showIndentedInstr i@(Define _) = showInstr i
showIndentedInstr i = "\t" <> showInstr i

showInstr :: Instr IRReg -> T.Text
showInstr (Load rd a) = "ldr " <> showOps [rd, a]
showInstr (Store rd a) = "str " <> showOps [rd, a]
showInstr (Mov rd o2) = "mov " <> showOps [rd, o2]
showInstr (Add rd rn o2) = "add " <> showOps [rd, rn, o2]
showInstr (Sub rd rn o2) = "sub " <> showOps [rd, rn, o2]
showInstr (Mul rd rm rs) = "mul " <> showOps [rd, rm, rs]
showInstr (Div {}) = "@ TODO - div"
showInstr (Cmp rn o2) = "cmp " <> showOps [rn, o2]
showInstr (Jmp l) = "b " <> l
showInstr (Jsr l) = "bl " <> l
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

showOps :: [Operand IRReg] -> T.Text
showOps = T.intercalate ", " . map showOp

showOp :: Operand IRReg -> T.Text
showOp (Reg r) = T.toLower (T.pack (show r))
showOp (Regs rs) = T.intercalate ", " (map (showOp . Reg) rs)
showOp (Imm i) = "#" <> T.pack (show i)
showOp (Abs i) = "=" <> i
showOp (Ind r) = "[" <> showOp (Reg r) <> "]"
showOp (ImmOffset r i) = "[" <> showOp (Reg r) <> ", " <> showOp (Imm i) <> "]"
