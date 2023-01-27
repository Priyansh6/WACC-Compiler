module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void

import System.Environment

type Parser = Parsec Void String

main :: IO ()
main = do 
  (fname:_) <- getArgs
  contents <- readFile fname
  parseTest (char 'a' :: Parser Char) contents