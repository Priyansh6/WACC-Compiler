{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL.Print (module REPL.Print) where

import qualified AST
import Control.Monad.Except (liftIO, throwError)
import Control.Monad.State (gets)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Interpreter.Identifiers (lookupVarOrParam)
import Interpreter.Utils (Aux (..), HeapValue (..), Interpreter, Value (..), lookupHeap)
import Semantic.Errors (SemanticError (VariableNotDefined), cyan, printSemanticErrors, reset)
import System.Console.Haskeline (InputT)

banner :: T.Text
banner =
  T.pack cyan
    <> T.unlines
      [ "Welcome to WACC 40's REPL",
        "\tCtrl+C\tinterrupt",
        "\tquit\texit",
        "\tCtrl+D\texit",
        "\ttab\tautocomplete wacc keywords",
        "\t:\tpreview value of any identifier, eg :print",
        "\thelp\tDisplay this message"
      ]
    <> T.pack reset

print :: T.Text -> Either SemanticError b -> InputT IO ()
print input result = case result of
  Left semanticErr -> do
    liftIO $ printSemanticErrors [semanticErr] input "REPL"
  Right _ -> return ()

printIdent :: AST.Ident -> Interpreter ()
printIdent ident =
  gets (M.lookup ident . funcs)
    >>= ( \case
            Just f@(AST.Func {}) -> return $ printFunction f
            Nothing -> do
              gets (lookupVarOrParam ident) >>= \case
                (Just val) -> showReplValue val
                _ -> printDefaultFunction ident
        )
    >>= liftIO . TIO.putStrLn . (\t -> T.pack cyan <> t <> T.pack reset)

printFunction :: AST.Func -> T.Text
printFunction (AST.Func wt (AST.Ident name _) ps _ _ _) =
  printWType wt
    <> " "
    <> name
    <> "("
    <> T.intercalate ", " (map (\(wt', AST.Ident name' _) -> printWType wt' <> " " <> name') ps)
    <> ")"

defaultFuncTypes :: M.Map T.Text T.Text
defaultFuncTypes =
  M.fromList
    [ ("read", "Reads an int or character into an existing ident\n\tint x = 1;\n\tread x;"),
      ("print", "Prints an expression\n\tprint 25+3;"),
      ("println", "Prints an expression with a new line\n\tchar[] x = ['a','b'];println x;"),
      ("newpair", "Initialise a new pair\n\tpair(int, bool) = newpair(4, true);"),
      ("len", "Get the array length of an identifier\n\tbool[] bs = [true, false];\n\tint l = len bs;"),
      ("ord", "Get the ASCII number corresponding to a given char\n\tbool b = ord 'a' == 97;"),
      ("chr", "Get the corresponding ASCII character for given integer\n\tchar c = chr 97;"),
      ("exit", "Exit the program with an integer exit code.\n\texit 255;"),
      ("return", "Terminate the function and set the return value\n\tbool f() is\n\t  return false\n\tend")
    ]

printDefaultFunction :: AST.Ident -> Interpreter T.Text
printDefaultFunction ident@(AST.Ident name _) = do
  case M.lookup name defaultFuncTypes of
    Just helperText -> return helperText
    Nothing -> throwError $ VariableNotDefined ident

showReplValue :: Value -> Interpreter T.Text
showReplValue arr'@(IArr _) = do
  hArr <- lookupHeap arr'
  case hArr of
    (HPair _ _) -> error "pair not valid array" -- never reaches
    (HArr arr) -> do
      elems <- mapM showReplValue arr
      return $ "[" <> T.intercalate ", " elems <> "]"
showReplValue pair'@(IPair _) = do
  hPair <- lookupHeap pair'
  case hPair of
    (HArr _) -> error "array is not valid pair" -- never reaches
    (HPair v1 v2) -> do
      left <- showReplValue v1
      right <- showReplValue v2
      return $ "(" <> left <> ", " <> right <> ")"
showReplValue v = return $ T.pack $ show v

printWType :: AST.WType -> T.Text
printWType AST.WUnit = "pair"
printWType AST.WInt = "int"
printWType AST.WChar = "char"
printWType AST.WBool = "bool"
printWType AST.WStr = "str"
printWType (AST.WArr base _) = printWType base <> "[]"
printWType (AST.WPair f s) = "pair(" <> printWType f <> ", " <> printWType s <> ")"
