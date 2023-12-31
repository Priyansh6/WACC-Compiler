module Syntax.ProgramConstructors 
  ( mkFunc,
    mkProgram
  ) 
where

import Text.Megaparsec ((<?>), label, try)

import qualified AST
import Syntax.Parser (Parser, liftPosScopeFunc)

mkProgram :: Parser [AST.Func] -> Parser AST.Stats -> Parser AST.Program
mkProgram funcs stats = AST.Program <$> funcs <*> stats

mkFunc :: Parser AST.WType -> Parser AST.Ident -> Parser [(AST.WType, AST.Ident)] -> Parser AST.Stats -> Parser AST.Func
mkFunc t i pl xs = label "function" $ try $ liftPosScopeFunc AST.Func (t <?> "return type") i (pl <?> "parameter list") (xs <?> "function body") >>= isValidFunc
  where
    isValidFunc :: AST.Func -> Parser AST.Func
    isValidFunc f@(AST.Func _ _ _ ys _ _)
      | validThroughAllPaths (last ys) = pure f
      | otherwise = fail "All paths through function must end with either a return or exit"

    validThroughAllPaths :: AST.Stat -> Bool
    validThroughAllPaths (AST.Return _ _) = True
    validThroughAllPaths (AST.Exit _ _) = True
    validThroughAllPaths (AST.If _ ys _ ys' _ _) = validThroughAllPaths (last ys) && validThroughAllPaths (last ys')
    validThroughAllPaths (AST.While _ ys _ _) = validThroughAllPaths (last ys)
    validThroughAllPaths (AST.Begin ys _) = validThroughAllPaths (last ys)
    validThroughAllPaths _ = False