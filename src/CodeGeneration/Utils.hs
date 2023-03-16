{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Utils
  ( Aux (..),
    IRSectionGenerator,
    IRStatementGenerator,
    LiterTable,
    numParamRegs,
    numGeneralRegs,
    addHelperFunc,
    exprType,
    getIdentType,
    getVarReg,
    getFuncReturnFuncName,
    getWType,
    insertVarReg,
    makeRegAvailable,
    makeRegsAvailable,
    withReg,
    nextFreeReg,
    nextLabel,
    stackTypeSize,
    heapTypeSize,
    wrapScope,
    temps, 
    used, 
    defs,
    (<++>),
    (++>),
    (<++),
  )
where

import AST hiding (Ident)
import qualified AST (Ident (Ident))
import CodeGeneration.Intermediate.Helpers
import CodeGeneration.Intermediate.IR
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor ((<&>))
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Semantic.Rename.Scope ( ScopeMap )
import Semantic.Type.SymbolTable (IdentType, SymbolTable, fromIdentType)

type IRStatementGenerator a = StateT Aux (Reader (SymbolTable, ScopeMap)) a

type IRSectionGenerator a = (Reader (SymbolTable, ScopeMap)) a

type LiterTable = M.Map T.Text Label

data Aux = Aux
  { available :: [IRReg],
    inUse :: [IRReg],
    labelId :: Int,
    sectionName :: T.Text,
    varLocs :: M.Map Ident IRReg,
    literTable :: LiterTable,
    helperFuncs :: HelperFuncs
  }

numParamRegs :: Int
numParamRegs = 4

numGeneralRegs :: Int
numGeneralRegs = 4

addHelperFunc :: HelperFunc -> IRStatementGenerator ()
addHelperFunc hf = modify (\a@(Aux {helperFuncs = hfs}) -> a {helperFuncs = insertHelperFunc hf hfs})

nextFreeReg :: IRStatementGenerator IRReg
nextFreeReg = state (\a@Aux {available = (nxt : rst), inUse = rs} -> (nxt, a {available = rst, inUse = nxt : rs}))

makeRegAvailable :: IRReg -> IRStatementGenerator ()
makeRegAvailable r = modify (\a@Aux {available = rs, inUse = rs'} -> a {available = r : rs, inUse = rs' L.\\ [r]})

makeRegsAvailable :: [IRReg] -> IRStatementGenerator ()
makeRegsAvailable = mapM_ makeRegAvailable

withReg :: (IRReg -> IRStatementGenerator a) -> IRStatementGenerator a
withReg f = nextFreeReg >>= (\r -> f r <* makeRegAvailable r)

nextLabel :: IRStatementGenerator Label
nextLabel = nextLabelId >>= toLabel
  where
    nextLabelId :: StateT Aux (Reader (SymbolTable, ScopeMap)) Int
    nextLabelId = state (\a@Aux {labelId = l} -> (l, a {labelId = l + 1}))

    toLabel :: Int -> StateT Aux (Reader (SymbolTable, ScopeMap)) Label
    toLabel i = gets (\Aux {sectionName = x} -> x <> "_l" <> T.pack (show i))

getFuncReturnFuncName :: IRStatementGenerator Label
getFuncReturnFuncName = gets (\Aux {sectionName = x} -> x <> "_return")

insertVarReg :: Ident -> IRReg -> IRStatementGenerator ()
insertVarReg i r = modify (\a@Aux {varLocs = vl} -> a {varLocs = M.insert i r vl})

getVarReg :: Ident -> IRStatementGenerator IRReg
getVarReg i = gets (\Aux {varLocs = vl} -> vl M.! i)

getIdentType :: Ident -> IRStatementGenerator IdentType
getIdentType (Ident i) = asks ((M.! AST.Ident i (0, 0)) . fst)

getWType :: Ident -> IRStatementGenerator WType
getWType = (<&> fromIdentType) . getIdentType

(<++>) :: Applicative m => m [a] -> m [a] -> m [a]
a <++> b = (++) <$> a <*> b

infixr 5 <++>

(++>) :: Applicative m => [a] -> m [a] -> m [a]
a ++> b = (++) a <$> b

infixr 5 ++>

(<++) :: Applicative m => m [a] -> [a] -> m [a]
a <++ b = (++) <$> a <*> pure b

infixr 5 <++

stackTypeSize :: WType -> Int
stackTypeSize WUnit = 4
stackTypeSize WInt = 4
stackTypeSize WBool = 4
stackTypeSize WChar = 4
stackTypeSize WStr = 4
stackTypeSize (WArr _ _) = 4
stackTypeSize (WPair _ _) = 8

heapTypeSize :: WType -> Int
heapTypeSize WUnit = 4
heapTypeSize WInt = 4
heapTypeSize WBool = 1
heapTypeSize WChar = 1
heapTypeSize WStr = 4
heapTypeSize (WArr _ _) = 4
heapTypeSize (WPair WUnit WUnit) = 4
heapTypeSize (WPair _ _) = 8

exprType :: Expr -> IRStatementGenerator WType
exprType (IntLiter _ _) = return WInt
exprType (BoolLiter _ _) = return WBool
exprType (CharLiter _ _) = return WChar
exprType (StrLiter _ _) = return WStr
exprType (PairLiter _) = return $ WPair WUnit WUnit
exprType (IdentExpr (AST.Ident i _) _) = getWType (Ident i)
exprType (ArrayExpr (ArrayElem _ [] _) _) = error "Can't have empty index in ArrayElem"
exprType (ArrayExpr (ArrayElem (AST.Ident i _) is _) _) = do
  wType <- getWType (Ident i)
  case wType of
    WArr t dim -> if dim == length is then return t else return $ WArr t (dim - length is)
    _ -> error "Can't be non-array type"
exprType (Not _ _) = return WBool
exprType (Neg _ _) = return WInt
exprType (Len _ _) = return WInt
exprType (Ord _ _) = return WInt
exprType (Chr _ _) = return WChar
exprType ((:*:) {}) = return WInt
exprType ((:/:) {}) = return WInt
exprType ((:%:) {}) = return WInt
exprType ((:+:) {}) = return WInt
exprType ((:-:) {}) = return WInt
exprType ((:>:) {}) = return WBool
exprType ((:>=:) {}) = return WBool
exprType ((:<:) {}) = return WBool
exprType ((:<=:) {}) = return WBool
exprType ((:==:) {}) = return WBool
exprType ((:!=:) {}) = return WBool
exprType ((:&&:) {}) = return WBool
exprType ((:||:) {}) = return WBool

wrapScope :: Int -> IRInstrs -> IRSectionGenerator IRInstrs
wrapScope scopeId instrs = do
  (_, sm) <- ask
  case M.lookup scopeId sm of
    Nothing -> return instrs
    Just ids -> do
      let stackSize = 4 * length ids
      return $ [Sub (Reg IRSP) (Reg IRSP) (Imm stackSize)] ++ instrs ++ [Add (Reg IRSP) (Reg IRSP) (Imm stackSize)]

tmpOperands :: Operand IRReg -> Set IRReg
tmpOperands (Reg r) = tmpReg r
tmpOperands (Regs rs) = foldr ((<>) . tmpReg) Set.empty rs
tmpOperands (Ind r) = tmpReg r
tmpOperands (ImmOffset r _) = tmpReg r
tmpOperands (ASR r _) = tmpReg r
tmpOperands _ = Set.empty

tmpReg :: IRReg -> Set IRReg
tmpReg t@(TmpReg _) = Set.singleton t
tmpReg _ = Set.empty

-- Returns the tmpregs defined by an instr
defs :: Instr IRReg -> Set IRReg
defs (Load o _) = tmpOperands o
defs (LoadB o _) = tmpOperands o
defs (Store _ o) = tmpOperands o
defs (StoreB _ o) = tmpOperands o
defs (Mov o _) = tmpOperands o
defs (Add o _ _) = tmpOperands o
defs (Sub o _ _) = tmpOperands o
defs (Mul o _ _) = tmpOperands o
defs (Div o _ _) = tmpOperands o
defs (Mod o _ _) = tmpOperands o
defs (Pop o) = tmpOperands o
defs _ = Set.empty

-- Returns the tmpregs used by an instr
used :: Instr IRReg -> Set IRReg
used (Load _ o) = tmpOperands o
used (LoadB _ o) = tmpOperands o
used (Store o _) = tmpOperands o
used (StoreB o _) = tmpOperands o
used (Mov _ o) = tmpOperands o
used (Add _ o1 o2) = tmpOperands o1 <> tmpOperands o2
used (Sub _ o1 o2) = tmpOperands o1 <> tmpOperands o2
used (Mul _ o1 o2) = tmpOperands o1 <> tmpOperands o2
used (Div _ o1 o2) = tmpOperands o1 <> tmpOperands o2
used (Mod _ o1 o2) = tmpOperands o1 <> tmpOperands o2
used (Cmp o1 o2) = tmpOperands o1 <> tmpOperands o2
used (Push o) = tmpOperands o
used _ = Set.empty

-- Returns all tmpRegs an instruction interacts with
temps :: Instr IRReg -> Set IRReg
temps (Load o1 o2) = tmpOperands o1 <> tmpOperands o2
temps (LoadB o1 o2) = tmpOperands o1 <> tmpOperands o2
temps (Store o1 o2) = tmpOperands o1 <> tmpOperands o2
temps (StoreB o1 o2) = tmpOperands o1 <> tmpOperands o2
temps (Mov o1 o2) = tmpOperands o1 <> tmpOperands o2
temps (Add o1 o2 o3) = tmpOperands o1 <> tmpOperands o2 <> tmpOperands o3
temps (Sub o1 o2 o3) = tmpOperands o1 <> tmpOperands o2 <> tmpOperands o3
temps (Mul o1 o2 o3) = tmpOperands o1 <> tmpOperands o2 <> tmpOperands o3
temps (Div o1 o2 o3) = tmpOperands o1 <> tmpOperands o2 <> tmpOperands o3
temps (Mod o1 o2 o3) = tmpOperands o1 <> tmpOperands o2 <> tmpOperands o3
temps (Cmp o1 o2) = tmpOperands o1 <> tmpOperands o2
temps (Push o1) = tmpOperands o1
temps (Pop o1) = tmpOperands o1
temps _ = Set.empty
