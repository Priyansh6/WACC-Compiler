{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main (main) where

import qualified Lexer as L
import CodeGeneration.ARM.PrettyPrint (showArm)
import CodeGeneration.Utils (mapBodies)
import CodeGeneration.ARM.Peephole (peepholeOptimise)
import qualified CodeGeneration.ARM.Registers as ARM (transProg)
import qualified CodeGeneration.Intermediate.Optimisation.RegisterAllocation as Optim (allocRegisters)
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
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment
import System.Exit
import System.FilePath ( takeBaseName )
import Text.Megaparsec

data Option =  PeepholeOptim | RegOptim deriving (Eq)

main :: IO ()
main = do 
  getArgs >>= \case
    [] -> runRepl
    ["-h"] -> TIO.putStrLn $ T.unlines [
      "./compile\t\t\tUse the WACC REPL",
      "./compile -h\t\t\tUsage guide",
      "./compile -i [filename.wacc]\tExecute a WACC using an interpreter",
      "./compile [optimisations] [filename.wacc]\tCompile a WACC file into assembly with optimisations (o1 and/or o2)" ]
    ("-i":fname:_) -> interpretFile fname
    ("-o1":"-o2":fname:_) -> compile fname [PeepholeOptim, RegOptim]
    ("-o2":"-o1":fname:_) -> compile fname [PeepholeOptim, RegOptim]
    ("-o1":fname:_) -> compile fname [PeepholeOptim]
    ("-o2":fname:_) -> compile fname [RegOptim]
    (fname:_) -> compile fname []

compile :: FilePath -> [Option] -> IO ()
compile fname options = do
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
          let irOptimisations = if RegOptim `elem` options then mapBodies Optim.allocRegisters else id
          let irProg = irOptimisations $ runReader (IR.transProg renamedAST') (symbolTable, scopeMap)
          let armOptimisations = if PeepholeOptim `elem` options then peepholeOptimise else id
          let armProg = armOptimisations $ ARM.transProg irProg
          TIO.writeFile (takeBaseName fname ++ ".s") (showArm armProg) >> exitSuccess