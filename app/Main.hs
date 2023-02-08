{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Programs (pProgram)
import Renamer (rename)
import SymbolTable (checkProg)

import Text.Megaparsec
import System.Environment
import System.Exit
import qualified Data.Text.IO as TIO
import qualified Lexer as L

main :: IO ()
main = do 
  (fname:_) <- getArgs
  contents <- TIO.readFile fname
  let res = runParser (L.fully pProgram) fname contents
  case res of
    Left _ -> exitWith (ExitFailure 100)
    Right ast -> case rename ast of
                  ((_, []), renamedAST) -> print $ checkProg renamedAST
                  _ -> exitWith (ExitFailure 1)
