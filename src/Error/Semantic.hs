module Error.Semantic (module Error.Semantic) where

import AST
import Error.Colour
import Data.List (intercalate)

type Expectation = [WType]

type Actual = WType
type ReturnType = WType
type ParameterTypes = [WType]

data SemanticError
  = VariableAlreadyDefined Ident
  | FunctionAlreadyDefined Ident ReturnType ParameterTypes
  | VariableNotDefined Ident
  | FunctionNotDefined Ident ReturnType ParameterTypes
  | IncompatibleTypes Position Expectation Actual
  | NonSubscriptable Position
  | IllegalReturn Position
  | IllegalPairExchange Position
  deriving (Show, Eq)

semanticMessage :: SemanticError -> String
semanticMessage semErr = case semErr of
  VariableAlreadyDefined (Ident i _) -> "The variable " ++ bold (show i) ++ yellow ++ " is already defined" ++ "\n"
  FunctionAlreadyDefined (Ident i _) rt ts -> "The function " ++ bold (show i) ++ yellow ++ " with return type " ++ bold (showWType rt) ++ yellow ++ " and parameter types (" ++ (bold . intercalate ", " . map showWType) ts ++ yellow ++ ") is already defined" ++ "\n"
  VariableNotDefined (Ident i _) -> "The variable " ++ bold (show i) ++ yellow ++ " is not defined" ++ "\n"
  FunctionNotDefined (Ident i _) rt ts -> "The function " ++ bold (show i) ++ yellow ++ " with return type " ++ bold (showWType rt) ++ yellow ++ " and parameter types (" ++ (bold . intercalate ", " . map showWType) ts ++ yellow ++ ") is not defined" ++ "\n"
  IncompatibleTypes _ expecteds actual -> "Incompatible types\n\tExpected: " ++ bold (intercalate " or " (map showWType expecteds)) ++ yellow ++ "\n\tActual:   " ++ bold (showWType actual) ++ "\n"
  NonSubscriptable _ -> "The expression is not subscriptable\n"
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
  (WArr wt depth) -> showWType wt ++ concat (replicate depth "[]")
  (WPair (WPair WInt WInt) (WPair WInt WInt)) -> "any Pair"
  (WPair WUnit WUnit) -> "Pair"
  (WPair f s) -> "(" ++ showWType f ++ ", " ++ showWType s ++ ")"

pairErrorType :: WType
pairErrorType = WPair (WPair WInt WInt) (WPair WInt WInt)

arrayErrorType :: WType
arrayErrorType = WArr WUnit 0

-- Creates a nested array type with given dimensionality
getArrayErrorType :: Int -> WType -> WType
getArrayErrorType 0 t = t
getArrayErrorType n t = WArr (getArrayErrorType (n - 1) t) 0

semanticPosition :: SemanticError -> Position
semanticPosition (VariableAlreadyDefined (Ident _ pos)) = pos
semanticPosition (FunctionAlreadyDefined (Ident _ pos) _ _) = pos
semanticPosition (VariableNotDefined (Ident _ pos)) = pos
semanticPosition (FunctionNotDefined (Ident _ pos) _ _) = pos
semanticPosition (IncompatibleTypes pos _ _) = pos
semanticPosition (NonSubscriptable pos) = pos
semanticPosition (IllegalReturn pos) = pos
semanticPosition (IllegalPairExchange pos) = pos

incompatibleType :: Position -> [WType] -> WType -> WType -> SemanticError
incompatibleType pos validTypes t1 t2 = IncompatibleTypes pos validTypes $ if t1 `elem` validTypes then t2 else t1
