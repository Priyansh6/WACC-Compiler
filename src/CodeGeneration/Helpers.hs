{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Helpers (generateHelperFunc) where

import qualified Data.Text as T

import CodeGeneration.IR 

data HelperFunc = Print HelperType | Println | Read HelperType | ErrDivZero | ErrOverflow | ErrNull
data HelperType = HInt | HChar | HBool | HString | HPointer

printfLabel :: Label
printfLabel = "printf"

putsLabel :: Label
putsLabel = "puts"

fflushLabel :: Label
fflushLabel = "fflush"

scanfLabel :: Label
scanfLabel = "scanf"

exitLabel :: Label
exitLabel = "exit"

isErrHelperFunc :: HelperFunc -> Bool
isErrHelperFunc ErrDivZero = True
isErrHelperFunc ErrOverflow = True
isErrHelperFunc ErrNull = True
isErrHelperFunc _ = False

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
generateHelperFunc hf@(Read hType)
  = Section
    [ StringData strLabel (spaceIfChar <> showHelperOption hType) ]
    [ Function (showHelperLabel hf) False
      [
        Push (Reg IRLR),
        Store (Reg (IRParam 0)) (ImmOffset IRSP (-4)), -- TODO: Only push byte size
        Mov (Reg (IRParam 1)) (Reg IRSP),
        Load (Reg (IRParam 0)) (Abs strLabel),
        Jsr scanfLabel,
        Load (Reg (IRParam 0)) (Reg IRSP),
        Add (Reg IRSP) (Reg IRSP) (Imm 1),
        Pop (Reg IRPC)
      ]
    ]
  where
    strLabel = showStrLabel hf 0
    spaceIfChar = case hType of
      HChar -> " "
      _     -> ""
generateHelperFunc errFunc 
  | isErrHelperFunc errFunc 
    = Section
      [ StringData strLabel errMsg ]
      [ Function (showHelperLabel ErrDivZero) False
        [
          Load (Reg (IRParam 0)) (Abs strLabel),
          Jsr (showHelperLabel (Print HString)),
          Mov (Reg (IRParam 0)) (Imm 255),
          Jsr exitLabel
        ]
      ]
  | otherwise = error "Unsupported helper function for generation"
  where 
    strLabel = showStrLabel errFunc 0
    errMsg = case errFunc of
      ErrDivZero -> "fatal error: division or modulo by zero\n"
      ErrOverflow -> "fatal error: integer overflow or underflow occurred\n"
      ErrNull -> "fatal error: null pair deferenced or freed\n"

showStrLabel :: HelperFunc -> Int -> Label
showStrLabel hf i = ".L." <> showHelperLabel hf <> "_str" <> (T.pack . show) i

showHelperLabel :: HelperFunc -> Label
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
showHelperLabel _ = error "Unknown Helper Function Label"

showHelperOption :: HelperType -> T.Text
showHelperOption HInt = "%d"
showHelperOption HChar = "%c"
showHelperOption HBool = "%b"
showHelperOption HString = "%.*s"
showHelperOption HPointer = "%p"