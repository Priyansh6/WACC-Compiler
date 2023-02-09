{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser (sc)
import Programs (pProgram)
import Renamer (rename)
import SymbolTable (checkProg)

import Control.Monad.Except
import Control.Monad.Trans.State
import Text.Megaparsec
import Data.Void
import System.Environment
import System.Exit
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

main :: IO ()
main = do 
  (fname:_) <- getArgs
  contents <- TIO.readFile fname
  let res = runParser (sc *> pProgram <* eof) fname contents
  case res of
    Left _ -> exitWith (ExitFailure 100)
    Right ast -> case rename ast of
      ((_, []), renamedAST) -> case runExcept $ (flip runStateT) M.empty (checkProg renamedAST) of
        Left err -> print err >> exitWith (ExitFailure 200)
        Right _ -> exitSuccess
      ((_, errs), _) -> print errs >> exitWith (ExitFailure 200)