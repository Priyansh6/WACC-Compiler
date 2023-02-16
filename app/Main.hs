{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Programs (program)
import SemanticErrors (printSemanticErrors)
import Renamer (rename)
import CheckTypes (checkProg)

import Control.Monad.Except
import Control.Monad.Trans.State
import Text.Megaparsec
import System.Environment
import System.Exit
import System.FilePath ( takeBaseName )
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Lexer as L

defaultOutput :: T.Text
defaultOutput = ".data\n.text\n.global main\nmain:\n"

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
      ((_, []), renamedAST) -> case runExcept $ runStateT (checkProg renamedAST) M.empty of
        Left err -> printSemanticErrors [err] contents fname >> exitWith (ExitFailure 200)
        Right _ -> TIO.writeFile (takeBaseName fname ++ ".s") defaultOutput >> exitSuccess
      ((_, errs), _) -> printSemanticErrors errs contents fname >> exitWith (ExitFailure 200)
