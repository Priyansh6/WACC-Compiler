{-# LANGUAGE OverloadedStrings #-}

module REPL.Handler (module REPL.Handler) where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Interpreter.Identifiers
import Interpreter.Program (evalProgram)
import Interpreter.Statement
import Interpreter.Utils (Aux (..), defaultAux, runInterpreter)
import REPL.Autocomplete
import REPL.Print (banner, print, printIdent)
import REPL.Read (isMultiLine, read, readMultiLine)
import Semantic.Errors (SemanticError, bold)
import Syntax.Repl (ReplInput (..))
import System.Console.Haskeline
import Text.Megaparsec
import Prelude hiding (print, read)

runRepl :: IO ()
runRepl = do
  TIO.putStrLn banner
  runInputT replSettings $ withInterrupt $ promptInput defaultAux

promptInput :: Aux -> InputT IO ()
promptInput st = handleInterrupt (outputStrLn "Interrupt (To exit press Ctrl+D)" >> promptInput st) $ do
  minput <- getInputLine (bold "wacc> ")
  case minput of
    Nothing -> return ()
    Just "" -> promptInput st
    Just "help" -> outputStrLn (T.unpack banner) >> promptInput st
    Just "quit" -> return ()
    Just input -> readEvalPrintLoop st input

readEvalPrintLoop :: Aux -> String -> InputT IO ()
readEvalPrintLoop st input =
  case read input of
    Left syntaxErr ->
      if isMultiLine input syntaxErr
        then do
          readMultiLine input >>= readEvalPrintLoop st
        else do
          outputStrLn $ errorBundlePretty syntaxErr
          promptInput st
    Right replAST -> do
      result <- eval replAST st
      print (T.pack input) result
      loop result st

eval :: ReplInput -> Aux -> InputT IO (Either SemanticError Aux)
eval ast =
  runInterpreter
    ( case ast of
        (ReplFunc func) -> addFunction func
        (ReplStat stat) -> evalStatement 0 stat
        (ReplIdent ident) -> printIdent ident
        (ReplProgram prog) -> evalProgram prog
    )

loop :: Either a Aux -> Aux -> InputT IO ()
loop (Left _) prevSt = promptInput prevSt
loop (Right newSt) _ = promptInput newSt
