{-# LANGUAGE OverloadedStrings #-}

module Semantic.Errors (module Semantic.Errors) where

import Data.List (intercalate)
import qualified Data.Text as T
import System.FilePath

import AST

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
  | FunctionAlreadyDefined Ident WType [WType]
  | VariableNotDefined Ident
  | FunctionNotDefined Ident WType [WType]
  | IncompatibleTypes Position Expectation Actual
  | NonSubscriptable Position
  | IllegalReturn Position
  | IllegalPairExchange Position
  | Runtime RuntimeError Position
  deriving (Show, Eq)

data RuntimeError
  = IndexOutOfBounds
  | DivideByZero
  | NullDereference
  | IntegerOverflow
  | IntegerUnderflow
  deriving (Show, Eq)

printSemanticErrors :: [SemanticError] -> T.Text -> String -> IO ()
printSemanticErrors errs contents fname = do
  putStrLn $ concatMap printSemanticError errs
  where
    file = T.lines contents

    printSemanticError :: SemanticError -> String
    printSemanticError semErr = "\n" ++ previewLocation semErr ++ previewCode (getPosition semErr) ++ yellow ++ errorMessage semErr ++ reset

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
        getRow r = T.unpack (file !! min (length file - 1) (r - 1)) ++ "\n"

errorMessage :: SemanticError -> String
errorMessage semErr = case semErr of
  VariableAlreadyDefined (Ident i _) -> "The variable " ++ bold (show i) ++ yellow ++ " is already defined" ++ "\n"
  FunctionAlreadyDefined (Ident i _) rt ts -> "The function " ++ bold (show i) ++ yellow ++ " with return type " ++ bold (showWType rt) ++ yellow ++ " and parameter types (" ++ (bold . intercalate ", " . map showWType) ts ++ yellow ++ ") is already defined" ++ "\n"
  VariableNotDefined (Ident i _) -> "The variable " ++ bold (show i) ++ yellow ++ " is not defined" ++ "\n"
  FunctionNotDefined (Ident i _) rt ts -> "The function " ++ bold (show i) ++ yellow ++ " with return type " ++ bold (showWType rt) ++ yellow ++ " and parameter types (" ++ (bold . intercalate ", " . map showWType) ts ++ yellow ++ ") is not defined" ++ "\n"
  IncompatibleTypes _ expecteds actual -> "Incompatible types\n\tExpected: " ++ bold (intercalate " or " (map showWType expecteds)) ++ yellow ++ "\n\tActual:   " ++ bold (showWType actual) ++ "\n"
  NonSubscriptable _ -> "The expression is not subscriptable\n"
  IllegalReturn _ -> "Return statements outside of functions are not allowed\n" ++ reset
  IllegalPairExchange _ -> "Illegal exchange of values between pairs of unknown types\n" ++ reset
  Runtime IndexOutOfBounds _ -> "Index out of bounds\n" ++ reset
  Runtime DivideByZero _ -> "Dividing by zero\n" ++ reset
  Runtime NullDereference _ -> "Can't dereference a null pointer\n" ++ reset
  Runtime IntegerOverflow _ -> "Integer overflow\n" ++ reset
  Runtime IntegerUnderflow _ -> "Integer underflow\n" ++ reset

showWType :: WType -> String
showWType t = case t of
  WUnit -> ""
  WInt -> "Integer"
  WBool -> "Boolean"
  WChar -> "Character"
  WStr -> "String"
  (WArr WUnit _) -> "any Array"
  (WArr wt depth) -> showWType wt ++ concat (replicate depth "[]")
  (WPair (WPair WInt WInt) (WPair WInt WInt)) -> "any Pair"
  (WPair WUnit WUnit) -> "Pair"
  (WPair f s) -> "(" ++ showWType f ++ ", " ++ showWType s ++ ")"

getPosition :: SemanticError -> Position
getPosition (VariableAlreadyDefined (Ident _ pos)) = pos
getPosition (FunctionAlreadyDefined (Ident _ pos) _ _) = pos
getPosition (VariableNotDefined (Ident _ pos)) = pos
getPosition (FunctionNotDefined (Ident _ pos) _ _) = pos
getPosition (IncompatibleTypes pos _ _) = pos
getPosition (NonSubscriptable pos) = pos
getPosition (IllegalReturn pos) = pos
getPosition (IllegalPairExchange pos) = pos
getPosition (Runtime _ pos) = pos

semanticError :: Position -> [WType] -> WType -> WType -> SemanticError
semanticError pos validTypes t1 t2 = IncompatibleTypes pos validTypes $ if t1 `elem` validTypes then t2 else t1
