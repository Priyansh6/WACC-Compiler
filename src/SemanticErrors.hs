{-# LANGUAGE OverloadedStrings #-}

module SemanticErrors (module SemanticErrors) where

import AST
import qualified Data.Text as T

data SemanticErrorType
  = VariableAlreadyDefined
  | FunctionAlreadyDefined
  | VariableNotDefined
  | FunctionNotDefined
  deriving (Show, Eq)

data SemanticError = SemanticError SemanticErrorType Ident
  deriving (Show, Eq)

printSemanticErrors :: T.Text -> [SemanticError] -> IO ()
printSemanticErrors contents errs = putStrLn $ concatMap printSemanticError errs
  where
    file = T.lines contents

    printSemanticError :: SemanticError -> String
    printSemanticError (SemanticError errType ident@(Ident i pos)) = previewCode ident ++ errorName ++ show i ++ "\n"
      where
        errorName = case errType of
          VariableAlreadyDefined -> "Variable already defined: "
          FunctionAlreadyDefined -> "Function already defined: "
          VariableNotDefined -> "Variable not defined: "
          FunctionNotDefined -> "Function not defined: "

    previewCode :: Ident -> String
    previewCode (Ident i (line, col)) =
      "\t|\n" 
      ++ show line ++ "\t| " ++ T.unpack (file !! (line - 1)) 
      ++ "\n\t| " ++ replicate (col - 1) ' ' ++ "^\n"
