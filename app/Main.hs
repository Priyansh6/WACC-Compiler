{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main (main) where

import qualified Lexer as L
import CodeGeneration.ARM.PrettyPrint (showArm)
import qualified CodeGeneration.ARM.Registers as ARM (transProg)
import qualified CodeGeneration.Intermediate.Program as IR
import Error.PrettyPrint (printSemanticErrors, semanticExit, syntaxExit)
import REPL.Handler (runRepl)
import Interpreter.Program (interpretFile)
import Syntax.Program (program)
import Semantic.Rename.Program (rename)
import Semantic.Type.CheckTypes (checkProg)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.FilePath ( takeBaseName )
import Text.Megaparsec
import qualified Data.Map as M

main :: IO ()
main = do 
  getArgs >>= \case
    [] -> runRepl
    ("-i":fname:_) -> interpretFile fname
    (fname:_) -> do
      contents <- TIO.readFile fname
      case runParser (L.fully program) fname contents of
        Left err -> do
          putStrLn (errorBundlePretty err)
          exitWith syntaxExit
        Right ast -> case rename ast of
          Left errs -> printSemanticErrors errs contents fname >> exitWith semanticExit
          Right (scopeMap, renamedAST) -> case runExcept $ runStateT (checkProg renamedAST) M.empty of
            Left err -> printSemanticErrors [err] contents fname >> exitWith semanticExit
            Right (renamedAST', symbolTable) -> do
              let irProg = runReader (IR.transProg renamedAST') (symbolTable, scopeMap)
              let armProg = ARM.transProg irProg
              TIO.writeFile (takeBaseName fname ++ ".s") (showArm armProg) >> exitSuccess
