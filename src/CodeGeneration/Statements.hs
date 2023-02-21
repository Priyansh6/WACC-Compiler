module CodeGeneration.Statements (transStats) where

import Control.Monad.Reader
import Control.Monad.State

import CodeGeneration.IR (IRReg, Instrs, IRInstrs, IRInstr, FPOffsets)
import CodeGeneration.Utils ((<++>), (<++), (++>))
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)

import AST

-- Needs to carry around symbol table + scope map + offsets of any variable from fp
transStats :: Stats -> StateT FPOffsets (Reader (SymbolTable, ScopeMap)) IRInstrs
transStats ss = concat <$> mapM transStat ss 

-- Pattern match on nodes
transStat :: Stat -> StateT FPOffsets (Reader (SymbolTable, ScopeMap)) IRInstrs
transStat Skip = return []
transStat (DecAssign t i r _) = undefined
transStat (Assign l r _) = undefined
transStat (Read l _) = undefined
transStat (Free e _) = undefined
transStat (Return e _) = transExp e ++> [Pop (TmpReg 0)]
transStat (Exit e _) = undefined
transStat (Print e) = undefined
transStat (Println e) = undefined
transStat (If e ss Scope ss' Scope _) = undefined 
transStat (While e ss Scope _) = undefined
transStat (Begin ss _) = transStats ss

transExp :: Expr -> StateT FPOffsets (Reader (SymbolTable, ScopeMap)) IRInstrs
transExp (IntLiter x _) = Push (Imm x)
transExp (BoolLiter True _) = Push (Imm 1)
transExp (BoolLiter False _) = Push (Imm 0)
transExp (CharLiter c _) = Push (Imm (ord c))
transExp (StrLiter t _) = undefined
transExp (PairLiter _) = undefined
transExp (IdentExpr i _) = undefined
transExp (ArrayExpr ae _) = undefined
transExp (Not e _) = undefined
transExp (Neg e _) = undefined
transExp (Len e _) = undefined
transExp (Ord e _) = undefined
transExp (Chr e _) = undefined
transExp ((:*:) e e' _) = undefined
transExp ((:/:) e e' _) = undefined
transExp ((:%:) e e' _) = undefined
transExp ((:+:) e e' _) = undefined
transExp ((:-:) e e' _) = undefined
transExp ((:>:) e e' _) = undefined
transExp ((:>=:) e e' _) = undefined
transExp ((:<:) e e' _) = undefined
transExp ((:<=:) e e' _) = undefined
transExp ((:==:) e e' _) = undefined
transExp ((:!=:) e e' _) = undefined
transExp ((:&&:) e e' _) = undefined
transExp ((:||:) e e' _) = undefined