{-# LANGUAGE OverloadedStrings #-}

module SemanticErrors (module SemanticErrors) where

import AST
import Data.List (intercalate)
import qualified Data.Text as T
import System.FilePath

showColour :: Bool
showColour = True

color :: Int -> String
color n = if showColour then concat ["\ESC[", show n, "m"] else ""

reset, red, green, yellow, blue, cyan :: String
reset = color 0
red = color 31
green = color 32
yellow = color 33
blue = color 34
cyan = color 36

bold :: String -> String
bold s = color 1 ++ s ++ reset

pairErrorType :: WType
pairErrorType = WPair (WPair WInt WInt) (WPair WInt WInt)

arrayErrorType :: WType
arrayErrorType = WArr WUnit 0

-- Creates a nested array type with given dimensionality
getArrayErrorType :: Int -> WType -> WType
getArrayErrorType 0 t = t
getArrayErrorType n t = WArr (getArrayErrorType (n - 1) t) 0

type Expectation = [WType]

type Actual = WType

data SemanticError
  = VariableAlreadyDefined Ident
  | FunctionAlreadyDefined Ident
  | VariableNotDefined Ident
  | FunctionNotDefined Ident
  | IncompatibleTypes Position Expectation Actual
  | WrongArguments Position Ident Int Int
  | IllegalReturn Position
  | IllegalPairExchange Position
  deriving (Show, Eq)

printSemanticErrors :: [SemanticError] -> T.Text -> String -> IO ()
printSemanticErrors errs contents fname = putStrLn $ concatMap printSemanticError errs
  where
    file = T.lines contents

    printSemanticError :: SemanticError -> String
    printSemanticError semErr = previewLocation semErr ++ previewCode (getPosition semErr) ++ yellow ++ errorMessage semErr ++ reset

    previewLocation :: SemanticError -> String
    previewLocation semErr =
      cyan ++ "--> " ++ takeFileName fname ++ " (line " ++ show row ++ ")\n" ++ reset
      where
        (row, _) = getPosition semErr

    previewCode :: Position -> String
    previewCode (row, _) =
      border
        ++ "...\n"
        ++ (if row > 2 then cyan ++ show (row - 2) ++ " | " ++ reset ++ getRow (row - 2) else "")
        ++ (if row > 1 then cyan ++ show (row - 1) ++ " | " ++ reset ++ getRow (row - 1) else "")
        ++ red
        ++ line
        ++ " | "
        ++ getRow row
        ++ border
        ++ "\n"
        ++ reset
      where
        line = show row
        border = cyan ++ replicate (length line) ' ' ++ " | " ++ reset
        getRow :: Int -> String
        getRow r = T.unpack (file !! (r - 1)) ++ "\n"

errorMessage :: SemanticError -> String
errorMessage semErr = case semErr of
  VariableAlreadyDefined (Ident i _) -> "The variable " ++ bold (show i) ++ yellow ++ " is already defined" ++ "\n"
  FunctionAlreadyDefined (Ident i _) -> "The function " ++ bold (show i) ++ yellow ++ " is already defined" ++ "\n"
  VariableNotDefined (Ident i _) -> "The variable " ++ bold (show i) ++ yellow ++ " is not defined" ++ "\n"
  FunctionNotDefined (Ident i _) -> "The function " ++ bold (show i) ++ yellow ++ " is not defined" ++ "\n"
  IncompatibleTypes _ expecteds actual -> "Incompatible types\n\tExpected: " ++ bold (intercalate " or " (map showWType expecteds)) ++ yellow ++ "\n\tActual:   " ++ bold (showWType actual) ++ "\n"
  WrongArguments _ (Ident i _) expected actual ->
    "The function "
      ++ func
      ++ if actual < expected
        then " is missing " ++ bold (show (expected - actual)) ++ yellow ++ " arguments"
        else " takes " ++ bold (show expected) ++ yellow ++ " arguments but " ++ bold (show actual) ++ yellow ++ " were given"
    where
      func = bold (show i) ++ yellow
  IllegalReturn _ -> "Return statements outside of functions are not allowed\n" ++ reset
  IllegalPairExchange _ -> "Illegal exchange of values between pairs of unknown types\n" ++ reset

showWType :: WType -> String
showWType t = case t of
  WUnit -> ""
  WInt -> "Integer"
  WBool -> "Boolean"
  WChar -> "Character"
  WStr -> "String"
  (WArr WUnit _) -> "any Array"
  (WArr wt _) -> showWType wt ++ "[]"
  (WPair (WPair WInt WInt) (WPair WInt WInt)) -> "any Pair"
  (WPair WUnit WUnit) -> "Pair"
  (WPair f s) -> "(" ++ showWType f ++ ", " ++ showWType s ++ ")"

getPosition :: SemanticError -> Position
getPosition (VariableAlreadyDefined (Ident _ pos)) = pos
getPosition (FunctionAlreadyDefined (Ident _ pos)) = pos
getPosition (VariableNotDefined (Ident _ pos)) = pos
getPosition (FunctionNotDefined (Ident _ pos)) = pos
getPosition (IncompatibleTypes pos _ _) = pos
getPosition (WrongArguments pos _ _ _) = pos
getPosition (IllegalReturn pos) = pos
getPosition (IllegalPairExchange pos) = pos

semanticError :: Position -> [WType] -> WType -> WType -> SemanticError
semanticError pos validTypes t1 t2 = IncompatibleTypes pos validTypes $ if t1 `elem` validTypes then t2 else t1
