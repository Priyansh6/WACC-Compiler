{-# LANGUAGE OverloadedStrings #-}

module SemanticErrors (module SemanticErrors) where

import AST
import qualified Data.Text as T
import Data.List ( intercalate ) 

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

printSemanticErrors :: T.Text -> [SemanticError] -> IO ()
printSemanticErrors contents errs = putStrLn $ concatMap printSemanticError errs
  where
    file = T.lines contents

    printSemanticError :: SemanticError -> String
    printSemanticError semErr = previewCode (getPosition semErr) ++ errorMessage semErr

    previewCode :: Position -> String
    previewCode (line, col) =
      "\t|\n"
        ++ show line
        ++ "\t| "
        ++ T.unpack (file !! (line - 1))
        ++ "\n\t| "
        ++ replicate (col - 1) ' '
        ++ "^\n"

errorMessage :: SemanticError -> String
errorMessage semErr = case semErr of
  VariableAlreadyDefined (Ident i _) -> "Variable already defined: " ++ show i ++ "\n"
  FunctionAlreadyDefined (Ident i _) -> "Function already defined: " ++ show i ++ "\n"
  VariableNotDefined (Ident i _) -> "Variable not defined: " ++ show i ++ "\n"
  FunctionNotDefined (Ident i _) -> "Function not defined: " ++ show i ++ "\n"
  IncompatibleTypes _ expecteds actual -> "Incompatible types\nExpected: " ++ intercalate " or " (map showWType expecteds) ++ "\nActual: " ++ showWType actual ++ "\n"
  WrongArguments _ (Ident i _) expected actual -> "Wrong number of arguments in function " ++ show i ++ "\nExpected: " ++ show expected ++ "\nActual: " ++ show actual ++ "\n"
  IllegalReturn _ -> "Return outside of function is not allowed\n"
  IllegalPairExchange _ -> "Illegal exchange of values between pairs of unknown types\n"
  
showWType :: WType -> String
showWType t = case t of
  WUnit -> "Pair"
  WInt -> "Integer"
  WBool -> "Boolean"
  WChar -> "Character"
  WStr -> "String"
  (WArr WUnit _) -> "Array"
  (WArr wt _) -> "Array of " ++ showWType wt ++ "s"
  (WPair (WPair _ _) (WPair _ _)) -> "Pair"
  (WPair f s) -> "Pair of (" ++ showWType f ++ ", " ++ showWType s ++ ")"

getPosition :: SemanticError -> Position
getPosition (VariableAlreadyDefined (Ident _ pos)) = pos
getPosition (FunctionAlreadyDefined (Ident _ pos)) = pos
getPosition (VariableNotDefined (Ident _ pos)) = pos
getPosition (FunctionNotDefined (Ident _ pos)) = pos
getPosition (IncompatibleTypes pos _ _) = pos
getPosition (WrongArguments pos _ _ _) = pos
getPosition (IllegalReturn pos) = pos
getPosition (IllegalPairExchange pos) = pos
