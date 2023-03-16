{-# LANGUAGE LambdaCase #-}

module REPL.Read (module REPL.Read) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void (Void)
import qualified Lexer as L
import Semantic.Errors (bold)
import Syntax.Repl (ReplInput, repl)
import System.Console.Haskeline (InputT, getInputLine)
import Text.Megaparsec (ErrorItem (Label), ParseError (TrivialError), ParseErrorBundle (bundleErrors), runParser)

read :: String -> Either (ParseErrorBundle T.Text Void) ReplInput
read input = runParser (L.fully repl) "REPL" (T.pack input)

readMultiLine :: String -> InputT IO String
readMultiLine prevLines =
  getInputLine (bold "...\t")
    >>= ( \case
            Nothing -> return ""
            Just ":quit" -> return ""
            Just "" -> return prevLines
            Just line -> readMultiLine $ prevLines ++ '\n' : line
        )

isMultiLine :: String -> ParseErrorBundle T.Text Void -> Bool
isMultiLine input err =
  not (null input)
    && last input /= '\n'
    && ( case NonEmpty.head $ bundleErrors err of
           (TrivialError _ _ labels) ->
             any
               (`Set.member` labels)
               [ Label ('f' NonEmpty.:| "unction"),
                 Label ('i' NonEmpty.:| "f statement"),
                 Label ('e' NonEmpty.:| "lse statement"),
                 Label ('w' NonEmpty.:| "hile statement"),
                 Label ('b' NonEmpty.:| "egin statement"),
                 Label ('b' NonEmpty.:| "ranch")
               ]
           _ -> False
       )
