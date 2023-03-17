{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Error.PrettyPrint (module Error.PrettyPrint) where

import qualified Data.Text as T
import System.FilePath

import AST
import Error.Semantic
import Error.Runtime
import Error.Colour
import Control.Monad.Except (MonadError(..))
import GHC.IO.Exception (ExitCode(ExitFailure))

data WaccError = Semantic SemanticError | Runtime RuntimeError

syntaxExit, semanticExit, runtimeExit :: ExitCode
syntaxExit = ExitFailure 100
semanticExit = ExitFailure 200
runtimeExit = ExitFailure 255

semanticError :: MonadError WaccError m => SemanticError -> m a
semanticError e = throwError $ Semantic e

runtimeError :: MonadError WaccError m => RuntimeError -> m a
runtimeError e = throwError $ Runtime e

printSemanticErrors :: [SemanticError] -> T.Text -> String -> IO ()
printSemanticErrors errs = printErrors (map Semantic errs) 

printErrors :: [WaccError] -> T.Text -> String -> IO ()
printErrors errs contents fname = do
  putStrLn $ concatMap printError errs
  where
    file = T.lines contents

    printError :: WaccError -> String
    printError err = "\n" ++ previewLocation err ++ previewCode (getPosition err) ++ yellow ++ errorMessage err ++ reset

    previewLocation :: WaccError -> String
    previewLocation err =
      cyan ++ "--> " ++ takeFileName fname ++ " (line " ++ show row ++ ")\n" ++ reset
      where
        (row, _) = getPosition err

    previewCode :: Position -> String
    previewCode (row, _) =
      border
        ++ "...\n"
        ++ (if row > 2 then cyan ++ show (row - 2) ++ " | " ++ reset ++ getRow (row - 2) else "")
        ++ (if row > 1 then cyan ++ show (row - 1) ++ " | " ++ reset ++ getRow (row - 1) else "")
        ++ red
        ++ line
        ++ " | "
        ++ getRow row
        ++ border
        ++ "\n"
        ++ reset
      where
        line = show row
        border = cyan ++ replicate (length line) ' ' ++ " | " ++ reset
        getRow :: Int -> String
        getRow r = T.unpack (file !! min (length file - 1) (r - 1)) ++ "\n"

errorMessage :: WaccError -> String
errorMessage (Semantic err) = "SemanticError: " ++ semanticMessage err
errorMessage (Runtime err) = "RuntimeError: " ++ runtimeMessage err

getPosition :: WaccError -> Position
getPosition (Semantic err) = semanticPosition err
getPosition (Runtime err) = runtimePosition err
