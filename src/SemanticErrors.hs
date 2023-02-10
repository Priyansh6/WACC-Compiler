{-# LANGUAGE OverloadedStrings #-}

module SemanticErrors (module SemanticErrors) where

import AST
import qualified Data.Text as T

type Expectation = [WType]
type Actual = WType
data SemanticError
  = VariableAlreadyDefined Ident
  | FunctionAlreadyDefined Ident
  | VariableNotDefined Ident
  | FunctionNotDefined Ident
  | IncompatibleTypes Position Expectation Actual
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
  IncompatibleTypes _ [expected] actual -> "Incompatible types\nExpected: " ++ show expected ++ "\nActual: " ++ show actual ++ "\n"
  

getPosition :: SemanticError -> Position
getPosition (VariableAlreadyDefined (Ident _ pos)) = pos
getPosition (FunctionAlreadyDefined (Ident _ pos)) = pos
getPosition (VariableNotDefined (Ident _ pos)) = pos
getPosition (FunctionNotDefined (Ident _ pos)) = pos
getPosition (IncompatibleTypes pos _ _) = pos
