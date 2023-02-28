{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Lexer as L
import CodeGeneration.ARM.PrettyPrint (showArm)
import CodeGeneration.ARM.Registers (transProg, initAux)
import CodeGeneration.Program (transProg)
import Syntax.Program (program)
import Semantic.Errors (printSemanticErrors)
import Semantic.Rename.Program (rename)
import Semantic.Type.CheckTypes (checkProg)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.FilePath ( takeBaseName )
import Text.Megaparsec

main :: IO ()
main = do 
  (fname:_) <- getArgs
  contents <- TIO.readFile fname
  let res = runParser (L.fully program) fname contents
  case res of
    Left err -> do
      putStrLn (errorBundlePretty err)
      exitWith (ExitFailure 100)
    Right ast -> case rename ast of
      ((scopeMap, []), renamedAST) -> case runExcept $ execStateT (checkProg renamedAST) M.empty of
        Left err -> printSemanticErrors [err] contents fname >> exitWith (ExitFailure 200)
        Right symbolTable -> do
          let irProg = runReader (CodeGeneration.Program.transProg renamedAST) (symbolTable, scopeMap)
          let armProg = evalState (CodeGeneration.ARM.Registers.transProg irProg) (CodeGeneration.ARM.Registers.initAux)
          TIO.writeFile (takeBaseName fname ++ ".s") (showArm armProg) >> exitSuccess
      ((_, errs), _) -> printSemanticErrors errs contents fname >> exitWith (ExitFailure 200)
