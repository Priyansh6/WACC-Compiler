{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Utils
  ( Aux (..),
    IRSectionGenerator,
    IRStatementGenerator,
    LiterTable,
    addHelperFunc,
    exprType,
    getIdentType,
    getVarReg,
    getWType,
    insertVarReg,
    makeRegAvailable,
    makeRegsAvailable,
    nextFreeReg,
    nextLabel,
    typeSize,
    (<++>),
    (++>),
    (<++),
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor ((<&>))
import qualified Data.Map as M
import qualified Data.Text as T

import AST hiding (Ident)
import qualified AST (Ident (Ident))
import CodeGeneration.Helpers
import CodeGeneration.IR
import Semantic.Rename.Scope
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

addHelperFunc :: HelperFunc -> IRStatementGenerator ()
addHelperFunc hf = modify (\a@(Aux {helperFuncs = hfs}) -> a {helperFuncs = insertHelperFunc hf hfs})

nextFreeReg :: IRStatementGenerator IRReg
nextFreeReg = state (\a@Aux {available = (nxt:rst), inUse = rs} -> (nxt, a {available = rst, inUse = nxt:rs}))

makeRegAvailable :: IRReg -> IRStatementGenerator ()
makeRegAvailable r = modify (\a@Aux {available = rs, inUse = (_:rs')} -> a {available = r:rs, inUse = rs'})

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
exprType (IdentExpr (AST.Ident i _) _) = getWType (Ident i)
exprType (ArrayExpr (ArrayElem (AST.Ident i _) _ _) _) = getWType (Ident i)
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