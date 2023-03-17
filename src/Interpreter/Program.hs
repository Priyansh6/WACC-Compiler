module Interpreter.Program (module Interpreter.Program) where

import qualified AST
import Control.Monad.Except (liftIO)
import qualified Data.Text.IO as TIO
import Interpreter.Identifiers (addFunction)
import Interpreter.Statement (evalStatements)
import Interpreter.Utils (Interpreter, Scope (..), defaultAux, runInterpreter)
import qualified Lexer as L
import Semantic.Errors (SemanticError (..), printSemanticErrors)
import Syntax.Program (program)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Text.Megaparsec (errorBundlePretty, runParser)

mainScope :: Scope
mainScope = Scope True 0

evalProgram :: AST.Program -> Interpreter ()
evalProgram (AST.Program fs ss) = do
  mapM_ addFunction fs
  _ <- evalStatements mainScope ss
  return ()

syntaxError, semanticError, runtimeError :: ExitCode
syntaxError = ExitFailure 100
semanticError = ExitFailure 200
runtimeError = ExitFailure 255

interpretFile :: FilePath -> IO ()
interpretFile fname = do
  contents <- TIO.readFile fname
  let res = runParser (L.fully program) fname contents
  case res of
    Left err -> do
      putStrLn (errorBundlePretty err)
      exitWith syntaxError
    Right ast -> do
      output <- runInterpreter (evalProgram ast) defaultAux
      case output of
        Left semanticErr -> do
          liftIO $ printSemanticErrors [semanticErr] contents fname
          case semanticErr of
            Runtime _ _ -> exitWith runtimeError
            _ -> exitWith semanticError
        Right _ -> exitSuccess