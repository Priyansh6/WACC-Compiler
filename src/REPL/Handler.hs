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
import REPL.Print (banner, print, printIdent, printIdentType)
import REPL.Read (isMultiLine, read, readMultiLine)
import Syntax.Repl (ReplInput (..))
import System.Console.Haskeline
import Text.Megaparsec ( errorBundlePretty )
import Prelude hiding (print, read)
import Error.PrettyPrint (WaccError)
import Error.Colour ( bold )

runRepl :: IO ()
runRepl = do
  TIO.putStrLn banner
  runInputT replSettings $ withInterrupt $ promptInput defaultAux

promptInput :: Aux -> InputT IO ()
promptInput st = handleInterrupt (outputStrLn "Interrupt (Ctrl+D or \"exit\" to exit)" >> promptInput st) $ do
  minput <- getInputLine (bold "wacc> ")
  case minput of
    Nothing -> return ()
    Just "" -> promptInput st
    Just ";" -> promptInput st
    Just "help" -> outputStrLn (T.unpack banner) >> promptInput st
    Just "exit" -> return ()
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

eval :: ReplInput -> Aux -> InputT IO (Either WaccError Aux)
eval ast =
  runInterpreter
    ( case ast of
        (ReplProgram prog) -> evalProgram prog
        (ReplStat stat) -> evalStatement 0 stat
        (ReplFunc func) -> addFunction func
        (ReplIdent ident) -> printIdent ident
        (ReplWType ident) -> printIdentType ident
    )

loop :: Either a Aux -> Aux -> InputT IO ()
loop (Left _) prevSt = promptInput prevSt
loop (Right newSt) _ = promptInput newSt
