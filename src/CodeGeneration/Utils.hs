{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Utils 
  ( IRSectionGenerator,
    IRStatementGenerator,
    Aux(Aux, available, labelId, varLocs, sectionName, literTable),
    intSize,
    maxRegSize,
    nextFreeReg,
    makeRegAvailable,
    makeRegsAvailable,
    nextLabel,
    insertVarReg,
    getVarReg,
    getVarType,
    exprType,
    typeSize,
    (<++>),
    (++>),
    (<++),
    LiterTable
  )
where

import AST hiding (Ident)
import CodeGeneration.IR
import Control.Monad.Reader
import Control.Monad.State
import Data.Map ((!))
import Semantic.Type.SymbolTable
import Semantic.Rename.Scope

import qualified AST (Ident(Ident))
import qualified Data.Map as M
import qualified Data.Text as T

type IRStatementGenerator a = StateT Aux (Reader (SymbolTable, ScopeMap)) a
type IRSectionGenerator a = (Reader (SymbolTable, ScopeMap)) a

type LiterTable = M.Map T.Text Label

data Aux = Aux { 
  available :: [IRReg],
  labelId :: Int,
  sectionName :: T.Text,
  varLocs :: M.Map Ident IRReg,
  literTable :: LiterTable
  }

maxRegSize :: Int
maxRegSize = 4

intSize :: Int
intSize = 4

nextFreeReg :: IRStatementGenerator IRReg
nextFreeReg = state (\a@Aux {available = (nxt:rst)} -> (nxt, a {available = rst}))

makeRegAvailable :: IRReg -> IRStatementGenerator ()
makeRegAvailable r = modify (\a@Aux {available = rs} -> a {available = r:rs})

makeRegsAvailable :: [IRReg] -> IRStatementGenerator ()
makeRegsAvailable = mapM_ makeRegAvailable

nextLabel :: IRStatementGenerator Label
nextLabel = nextLabelId >>= toLabel
  where
    nextLabelId :: StateT Aux (Reader (SymbolTable, ScopeMap)) Int
    nextLabelId = state (\a@Aux {labelId = l} -> (l, a {labelId = l + 1}))

    toLabel :: Int -> StateT Aux (Reader (SymbolTable, ScopeMap)) Label
    toLabel i = gets (\Aux {sectionName = x} -> x <> "_l" <> T.pack (show i))

insertVarReg :: Ident -> IRReg -> IRStatementGenerator ()
insertVarReg i r = modify (\a@Aux {varLocs = vl} -> a {varLocs = M.insert i r vl})

getVarReg :: Ident -> IRStatementGenerator IRReg
getVarReg i = gets (\Aux {varLocs = vl} -> vl ! i)

getVarType :: Ident -> IRStatementGenerator IdentType
getVarType (Ident i) = asks ((! AST.Ident i (0, 0)) . fst)

(<++>) :: Applicative m => m [a] -> m [a] -> m [a]
a <++> b = (++) <$> a <*> b
infixr 5 <++>

(++>) :: Applicative m => [a] -> m [a] -> m [a]
a ++> b = (++) a <$> b
infixr 5 ++>

(<++) :: Applicative m => m [a] -> [a] -> m [a]
a <++ b = (++) <$> a  <*> pure b
infixr 5 <++

typeSize :: WType -> Int
typeSize WUnit = 4
typeSize WInt = 4
typeSize WBool = 1
typeSize WChar = 1
typeSize WStr = error "size not known"
typeSize (WArr _ _) = error "size not known"
typeSize (WPair _ _) = 8

exprType :: Expr -> IRStatementGenerator WType
exprType (IntLiter _ _) = return WInt
exprType (BoolLiter _ _) = return WBool
exprType (CharLiter _ _) = return WChar
exprType (StrLiter _ _) = return WStr
exprType (PairLiter _) = return WUnit
exprType (IdentExpr (AST.Ident i _) _) = fromIdentType <$> getVarType (Ident i)
exprType (ArrayExpr (ArrayElem (AST.Ident i _) _ _) _) = fromIdentType <$> getVarType (Ident i)
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