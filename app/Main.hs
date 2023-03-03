{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Lexer as L
import CodeGeneration.ARM.PrettyPrint (showArm)
import qualified CodeGeneration.ARM.Registers as ARM (transProg)
import qualified CodeGeneration.Intermediate.Program as IR
import Syntax.Program (program)
import Semantic.Errors (printSemanticErrors)
import Semantic.Rename.Program (rename)
import Semantic.Type.CheckTypes (checkProg)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.FilePath ( takeBaseName )
import Text.Megaparsec

syntaxError, semanticError :: ExitCode
syntaxError = ExitFailure 100
semanticError = ExitFailure 200

main :: IO ()
main = do 
  (fname:_) <- getArgs
  contents <- TIO.readFile fname
  let res = runParser (L.fully program) fname contents
  case res of
    Left err -> do
      putStrLn (errorBundlePretty err)
      exitWith syntaxError
    Right ast -> case rename ast of
      ((scopeMap, []), renamedAST) -> case runExcept $ execStateT (checkProg renamedAST) M.empty of
        Left err -> printSemanticErrors [err] contents fname >> exitWith semanticError
        Right symbolTable -> do
          let irProg = runReader (IR.transProg renamedAST) (symbolTable, scopeMap)
          let armProg = ARM.transProg irProg
          TIO.writeFile (takeBaseName fname ++ ".s") (showArm armProg) >> exitSuccess
      ((_, errs), _) -> printSemanticErrors errs contents fname >> exitWith semanticError
