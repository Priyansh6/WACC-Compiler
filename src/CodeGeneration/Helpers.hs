{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.Helpers (module CodeGeneration.Helpers) where

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

import CodeGeneration.IR 
import AST

intSize :: Int
intSize = 4

maxRegSize :: Int
maxRegSize = 4

type HelperFuncs = S.Set HelperFunc

data HelperFunc
  = HPrint HelperType
  | HPrintln
  | HRead HelperType
  | FreePair
  | ArrStore   
  | ArrLoad
  | BoundsCheck
  | ErrDivZero
  | ErrOverflow
  | ErrNull
  deriving (Ord, Eq)

data HelperType = HInt | HChar | HBool | HString | HPointer deriving (Ord, Eq)

fromWType :: WType -> HelperType
fromWType WUnit = HInt
fromWType WInt = HInt
fromWType WBool = HBool
fromWType WChar = HChar
fromWType WStr = HString
fromWType (WArr _ _) = HPointer
fromWType (WPair _ _) = HPointer

dependencyMap :: M.Map HelperFunc [HelperFunc]
dependencyMap = M.fromList
  [
    (FreePair, [ErrNull]),
    (ArrStore, [BoundsCheck]),
    (ArrLoad, [BoundsCheck]),
    (ErrDivZero, [HPrint HString]),
    (ErrOverflow, [HPrint HString]),
    (ErrNull, [HPrint HString])
  ]

insertHelperFunc :: HelperFunc -> HelperFuncs -> HelperFuncs
insertHelperFunc hf hfs
  | hf `S.member` hfs = hfs
  | otherwise         = foldr insertHelperFunc (S.insert hf hfs) hfDependencies 
  where
    hfDependencies = M.findWithDefault [] hf dependencyMap

errorCode :: Int
errorCode = 255

printfLabel, putsLabel, fflushLabel, scanfLabel, exitLabel, freeLabel :: Label
printfLabel = "printf"
putsLabel = "puts"
fflushLabel = "fflush"
scanfLabel = "scanf"
exitLabel = "exit"
freeLabel = "free"

isArrHelperFunc :: HelperFunc -> Bool
isArrHelperFunc ArrStore = True
isArrHelperFunc ArrLoad = True
isArrHelperFunc _ = False

isErrHelperFunc :: HelperFunc -> Bool
isErrHelperFunc BoundsCheck = True
isErrHelperFunc ErrDivZero = True
isErrHelperFunc ErrOverflow = True
isErrHelperFunc ErrNull = True
isErrHelperFunc _ = False

generateHelperFuncs :: HelperFuncs -> [Section IRReg]
generateHelperFuncs hfs
  = map generateHelperFunc $ S.toList hfs

generateHelperFunc :: HelperFunc -> Section IRReg
generateHelperFunc hf@(HPrint HBool)
  = Section
    [ 
      StringData boolStr0 "false",
      StringData boolStr1 "true",
      StringData boolStr2 (showHelperOption HBool)
    ]
    (Body boolLabel False
      [ 
        Push (Regs [IRLR]),
        Cmp (Reg (IRParam 0)) (Imm 0),
        Jne boolLabel0,
        Load (Reg (IRParam 2)) (Abs boolStr0),
        Jmp boolLabel1,
        Define boolLabel0,
        Load (Reg (IRParam 2)) (Abs boolStr1),
        Define boolLabel1,
        Load (Reg (IRParam 1)) (ImmOffset (IRParam 2) (-maxRegSize)),
        Load (Reg (IRParam 0)) (Abs boolStr2),
        Jsr printfLabel,
        Mov (Reg (IRParam 0)) (Imm 0),
        Jsr fflushLabel,
        Pop (Regs [IRPC])
      ]
    )
    where
      boolLabel = showHelperLabel hf
      boolLabel0 = ".L" <> boolLabel <> "0"
      boolLabel1 = ".L" <> boolLabel <> "1"
      boolStr0 = showStrLabel hf 0
      boolStr1 = showStrLabel hf 1
      boolStr2 = showStrLabel hf 2
generateHelperFunc hf@(HPrint hType) 
  = Section 
    [ StringData strLabel (showHelperOption hType) ] 
    (Body (showHelperLabel hf) False $
      [ Push (Regs [IRLR]) ]
      ++ setupParams
      ++ [
        Load (Reg (IRParam 0)) (Abs strLabel),
        Jsr printfLabel,
        Mov (Reg (IRParam 0)) (Imm 0),
        Jsr fflushLabel,
        Pop (Regs [IRPC])
      ]
    )
  where
    strLabel = showStrLabel hf 0
    setupParams = 
      case hType of
        HString -> [
                     Mov (Reg (IRParam 2)) (Reg (IRParam 0)),
                     Load (Reg (IRParam 1)) (ImmOffset (IRParam 0) (-intSize))
                   ]
        _       -> [ Mov (Reg (IRParam 1)) (Reg (IRParam 0)) ]
generateHelperFunc HPrintln
  = Section
    [ StringData strLabel "" ]
    (Body (showHelperLabel HPrintln) False
      [
        Push (Regs [IRLR]),
        Load (Reg (IRParam 0)) (Abs strLabel),
        Jsr putsLabel,
        Mov (Reg (IRParam 0)) (Imm 0),
        Jsr fflushLabel,
        Pop (Regs [IRPC])
      ]
    )
    where
      strLabel = showStrLabel HPrintln 0
generateHelperFunc hf@(HRead hType)
  = Section
    [ StringData strLabel (spaceIfChar <> showHelperOption hType) ]
    (Body (showHelperLabel hf) False
      [
        Push (Regs [IRLR]),
        Sub (Reg IRSP) (Reg IRSP) (Imm intSize),
        Store (Reg (IRParam 0)) (Reg IRSP),
        Mov (Reg (IRParam 1)) (Reg IRSP),
        Load (Reg (IRParam 0)) (Abs strLabel),
        Jsr scanfLabel,
        Load (Reg (IRParam 0)) (Reg IRSP),
        Add (Reg IRSP) (Reg IRSP) (Imm intSize),
        Pop (Regs [IRPC])
      ]
    )
  where
    strLabel = showStrLabel hf 0
    spaceIfChar = case hType of
      HChar -> " "
      _     -> ""
generateHelperFunc FreePair
  = Section [] 
    (Body (showHelperLabel FreePair) False 
      [
        Push (Regs [IRLR]),
        Mov (Reg IRLR) (Reg (IRParam 0)),
        Cmp (Reg IRLR) (Imm 0),
        Jle (showHelperLabel ErrNull),
        Load (Reg (IRParam 0)) (ImmOffset IRLR 0),
        Jsr freeLabel,
        Load (Reg (IRParam 0)) (ImmOffset IRLR maxRegSize),
        Jsr freeLabel,
        Mov (Reg (IRParam 0)) (Reg IRLR),
        Jsr freeLabel,
        Pop (Regs [IRPC])
      ]
    )
generateHelperFunc hf
  | isArrHelperFunc hf
    = Section []
      (Body (showHelperLabel hf) False
        [
          Push (Regs [IRLR]),
          Cmp (Reg (IRParam 1)) (Imm 0),
          Jl (showHelperLabel BoundsCheck),
          Load (Reg IRLR) (ImmOffset (IRParam 0) (-intSize)),
          Cmp (Reg (IRParam 1)) (Reg IRLR),
          Jge (showHelperLabel BoundsCheck),
          Mul (Reg (IRParam 1)) (Reg (IRParam 1)) (Imm maxRegSize),
          Add (Reg (IRParam 0)) (Reg (IRParam 0)) (Reg (IRParam 1)),
          arrInstr,
          Pop (Regs [IRPC])
        ]
      )
  | isErrHelperFunc hf 
    = Section
      [ StringData errStrLabel errMsg ]
      (Body (showHelperLabel hf) False $
        [ Load (Reg (IRParam 0)) (Abs errStrLabel) ]
        ++ errInstrs
        ++ [
          Mov (Reg (IRParam 0)) (Imm errorCode),
          Jsr exitLabel
        ]
      )
  | otherwise = error "Unsupported helper function for generation"
  where 
    arrInstr = case hf of
      ArrStore -> Store (Reg (IRParam 2)) (Reg (IRParam 0))
      ArrLoad  -> Load (Reg IRRet) (Reg (IRParam 0))
      _        -> error "Can't have non ArrStore or ArrLoad array helper function"
    errInstrs = case hf of
      BoundsCheck -> [
                       Jsr printfLabel,
                       Mov (Reg (IRParam 0)) (Imm 0),
                       Jsr fflushLabel
                     ]
      _           -> [ Jsr (showHelperLabel (HPrint HString)) ]
    errStrLabel = showStrLabel hf 0
    errMsg = case hf of
      BoundsCheck -> "fatal error: array index %d out of bounds\n"
      ErrDivZero  -> "fatal error: division or modulo by zero\n"
      ErrOverflow -> "fatal error: integer overflow or underflow occurred\n"
      ErrNull     -> "fatal error: null pair deferenced or freed\n"
      _           -> error "Can't generate error message for this helper function"

showStrLabel :: HelperFunc -> Int -> Label
showStrLabel hf i = ".L." <> showHelperLabel hf <> "_str" <> (T.pack . show) i

showHelperLabel :: HelperFunc -> Label
showHelperLabel (HPrint HInt)     = "_printi"
showHelperLabel (HPrint HChar)    = "_printc"
showHelperLabel (HPrint HBool)    = "_printb"
showHelperLabel (HPrint HString)  = "_prints"
showHelperLabel (HPrint HPointer) = "_printp"
showHelperLabel HPrintln          = "_println"
showHelperLabel (HRead HInt)      = "_readi"
showHelperLabel (HRead HChar)     = "_readc"
showHelperLabel FreePair         = "_freepair"
showHelperLabel ArrStore         = "_arrStore"
showHelperLabel ArrLoad          = "_arrLoad"
showHelperLabel BoundsCheck      = "_boundsCheck"
showHelperLabel ErrDivZero       = "_errDivZero"
showHelperLabel ErrOverflow      = "_errOverflow"
showHelperLabel ErrNull          = "_errNull"
showHelperLabel _                = error "Unknown Helper Function Label"

showHelperOption :: HelperType -> T.Text
showHelperOption HInt     = "%d"
showHelperOption HChar    = "%c"
showHelperOption HBool    = "%b"
showHelperOption HString  = "%.*s"
showHelperOption HPointer = "%p"