{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser (sc)
import Programs (pProgram)

import Text.Megaparsec
import Data.Void
import System.Environment
import System.Exit
import qualified Data.Text.IO as TIO

main :: IO ()
main = do 
  (fname:_) <- getArgs
  contents <- TIO.readFile fname
  let res = runParser (sc *> pProgram <* eof) fname contents
  case res of
    Left _ -> exitWith (ExitFailure 100)
    Right _ -> exitWith ExitSuccess