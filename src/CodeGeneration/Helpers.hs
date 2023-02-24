{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Helpers (generateHelperFunc) where

import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Text as T

import CodeGeneration.IR 
import Semantic.Rename.Scope (ScopeMap)
import Semantic.Type.SymbolTable (SymbolTable)
import qualified AST

data HelperFunc = Print HelperType | Println | Read HelperType | ErrDivZero | ErrOverflow | ErrNull
data HelperType = HInt | HChar | HBool | HString | HPointer
data FormatType = FInt | FStr 

type HelperGenerator = Reader (SymbolTable, ScopeMap) IRInstrs

-- Generates code for print, println, read etc.
-- should mayb (definitely) mangle names for these so these aren't renamed variables
generateHelperFunc :: HelperFunc -> IRInstrs
generateHelperFunc (Print hType)
  = [ StringData (showStrLabel (Print hType) 0) (showHelperOption hType), 
      Define (showHelperLabel (Print hType)) False,
      Push (Reg IRLR),
      Mov (Reg (TmpReg))
      ]

-- generateHelperFuncs = scanHelperFuncs >>= generateHelperFuncs' 
--     where
--         generateHelperFuncs' :: Set (HelperFunc, FormatType) -> Reader (SymbolTable, ScopeMap) IRInstrs
--         generateHelperFuncs' = undefined

scanHelperFuncs :: AST.Program -> Set (HelperFunc, FormatType)
scanHelperFuncs = undefined

showStrLabel :: HelperFunc -> Int -> T.Text
showStrLabel hf i = ".L." <> showHelperLabel hf <> "_str" <> (T.pack . show) i

showHelperLabel :: HelperFunc -> T.Text
showHelperLabel (Print HInt) = "_printi"
showHelperLabel (Print HChar) = "_printc"
showHelperLabel (Print HBool) = "_printb"
showHelperLabel (Print HString) = "_prints"
showHelperLabel (Print HPointer) = "_printp"
showHelperLabel (Println) = "_println"
showHelperLabel (Read HInt) = "_readi"
showHelperLabel (Read HChar) = "_readc"
showHelperLabel (ErrDivZero) = "_errDivZero"
showHelperLabel (ErrOverflow) = "_errOverflow"
showHelperLabel (ErrNull) = "_errNull"

showHelperOption :: HelperType -> T.Text
showHelperOption HInt = "%d"
showHelperOption HChar = "%c"
showHelperOption HBool = "%b"
showHelperOption HString = "%.*s"
showHelperOption HPointer = "%p"