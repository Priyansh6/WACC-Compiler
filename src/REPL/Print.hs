{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL.Print (module REPL.Print) where

import qualified AST
import Control.Monad.Except (liftIO)
import Control.Monad.State (gets)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Interpreter.Identifiers (lookupVarOrParam)
import Interpreter.Utils (Aux (..), Interpreter, Value (..), lookupHeapArray, lookupHeapPair)
import System.Console.Haskeline (InputT)
import Interpreter.Type (toWType)
import Error.PrettyPrint (WaccError, printErrors, semanticError)
import Error.Colour
import Error.Semantic ( SemanticError(..) )

printBanner :: IO ()
printBanner =
  replOutput $
    T.unlines
      [ "Welcome to WACC 40's REPL",
        "\tCtrl+C\tinterrupt",
        "\tCtrl+D\texit",
        "\ttab\tautocomplete wacc keywords",
        "\t: \tpreview value of any identifier, eg :print",
        "\t::\tpreview type of any identifier, eg ::myInt",
        "\thelp\tDisplay this message"
      ]

replOutput :: T.Text -> IO ()
replOutput = TIO.putStrLn . (\t -> T.pack cyan <> t <> T.pack reset)

print :: T.Text -> Either WaccError b -> InputT IO ()
print input result = case result of
  Left semanticErr -> do
    liftIO $ printErrors [semanticErr] input "REPL"
  Right _ -> return ()

getFuncs :: AST.Ident -> Aux -> [AST.Func]
getFuncs ident = M.elems . M.filterWithKey (\(i, _, _) _ -> i == ident) . funcs

printIdent :: AST.Ident -> Interpreter ()
printIdent ident = do
  fs <- gets (getFuncs ident)
  ( if null fs
      then do
        gets (lookupVarOrParam ident) >>= \case
          (Just val) -> showReplValue val
          _ -> printDefaultFunction ident
      else return $ printFunctions fs
    )
    >>= liftIO . replOutput

printFunctions :: [AST.Func] -> T.Text
printFunctions = T.unlines . map printFunction

printFunction :: AST.Func -> T.Text
printFunction (AST.Func wt (AST.Ident name _) ps _ _ _) =
  showType wt
    <> " "
    <> name
    <> "("
    <> T.intercalate ", " (map (\(wt', AST.Ident name' _) -> showType wt' <> " " <> name') ps)
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
    Nothing -> semanticError $ VariableNotDefined ident

showReplValue :: Value -> Interpreter T.Text
showReplValue (IArr addr) = do
  arr <- lookupHeapArray addr (1,1)
  elems <- mapM showReplValue arr
  return $ "[" <> T.intercalate ", " elems <> "]"
showReplValue (IPair addr) = do
  (v1, v2) <- lookupHeapPair addr (1,1)
  left <- showReplValue v1
  right <- showReplValue v2
  return $ "(" <> left <> ", " <> right <> ")"
showReplValue v = return $ T.pack $ show v

showType :: AST.WType -> T.Text
showType AST.WUnit = "pair"
showType AST.WInt = "int"
showType AST.WChar = "char"
showType AST.WBool = "bool"
showType AST.WStr = "str"
showType (AST.WArr base _) = showType base <> "[]"
showType (AST.WPair f s) = "pair(" <> showType f <> ", " <> showType s <> ")"

printIdentType :: AST.Ident -> Interpreter ()
printIdentType ident =
  gets (getFuncs ident) >>= \case
      [] ->
        gets (lookupVarOrParam ident) >>= \case
          (Just val) -> showType <$> toWType val
          _ -> printDefaultFunction ident
      fs -> return $ printFunctions fs
    >>= liftIO . replOutput
