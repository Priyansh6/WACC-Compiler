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

printfLabel :: Label
printfLabel = "printf"

putsLabel :: Label
putsLabel = "puts"

fflushLabel :: Label
fflushLabel = "fflush"

isHelperBool :: HelperType -> Bool
isHelperBool HBool = True
isHelperBool _     = False

-- Generates code for print, println, read etc.
-- should mayb (definitely) mangle names for these so these aren't renamed variables
generateHelperFunc :: HelperFunc -> Section IRReg
generateHelperFunc hf@(Print HBool)
  = Section
    [ 
      StringData boolStr0 "false",
      StringData boolStr1 "true",
      StringData boolStr2 (showHelperOption HBool)
    ]
    [ 
      Function (showHelperLabel hf) False
      [ 
        Push (Reg IRLR),
        Cmp (Reg (IRParam 0)) (Imm 0),
        Jne bool0,
        Load (Reg (IRParam 2)) (Abs boolStr0),
        Jmp bool1
      ],
      Function bool0 False [ Load (Reg (IRParam 2)) (Abs boolStr1) ],
      Function bool1 False 
      [
        Load (Reg (IRParam 1)) (ImmOffset (IRParam 2) (-4)),
        Load (Reg (IRParam 0)) (Abs boolStr2),
        Jsr printfLabel,
        Mov (Reg (IRParam 0)) (Imm 0),
        Jsr fflushLabel,
        Pop (Reg IRPC)
      ]
    ]
    where
      bool0 = showHelperOption HBool <> "0"
      bool1 = showHelperOption HBool <> "1"
      boolStr0 = showStrLabel hf 0
      boolStr1 = showStrLabel hf 1
      boolStr2 = showStrLabel hf 2
generateHelperFunc hf@(Print hType) 
  = Section 
    [ StringData strLabel (showHelperOption hType) ] 
    [ Function funcLabel False $
      [ Push (Reg IRLR) ]
      ++ setupParams
      ++ [
        Load (Reg (IRParam 0)) (Abs strLabel),
        Jsr printfLabel,
        Mov (Reg (IRParam 0)) (Imm 0),
        Jsr fflushLabel,
        Pop (Reg IRPC)
      ]
    ]
  where
    funcLabel = showHelperLabel hf
    strLabel = showStrLabel hf 0

    setupParams :: IRInstrs
    setupParams = 
      case hType of
        HString -> [
                     Mov (Reg (IRParam 2)) (Reg (IRParam 0)),
                     Load (Reg (IRParam 1)) (ImmOffset (IRParam 0) (-4))
                   ]
        _       -> [ Mov (Reg (IRParam 1)) (Reg (IRParam 0)) ]
generateHelperFunc Println
  = Section
    [ StringData strLabel "" ]
    [ Function (showHelperLabel Println) False
      [
        Push (Reg IRLR),
        Load (Reg (IRParam 0)) (Abs strLabel),
        Jsr putsLabel,
        Mov (Reg (IRParam 0)) (Imm 0),
        Jsr fflushLabel,
        Pop (Reg IRPC)
      ]
    ]
    where
      strLabel = showStrLabel Println 0



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