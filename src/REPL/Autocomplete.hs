module REPL.Autocomplete (module REPL.Autocomplete) where

import Data.List (isPrefixOf)
import Lexer (keywords)
import System.Console.Haskeline

searchIdentifiers :: String -> [Completion]
searchIdentifiers str = map simpleCompletion $ filter (str `isPrefixOf`) keywords

replSettings :: Settings IO
replSettings =
  Settings
    { historyFile = Just "./.wacc_history",
      complete = completeWord Nothing " \t" $ return . searchIdentifiers,
      autoAddHistory = True
    }