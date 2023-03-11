module UnitTest.Semantic.Test (test, testWithStack) where

import Control.Monad.Reader
import Control.Monad.State

import Semantic.Errors
import Semantic.Rename.Utils

test :: Aux -> (a -> Renamer a) -> a -> Either [SemanticError] a
test = testWithStack initScopeStack

testWithStack :: ScopeStack -> Aux -> (a -> Renamer a) -> a -> Either [SemanticError] a
testWithStack stack a renamer x
  | null es   = Right x'
  | otherwise = Left es
  where 
    es = errors a'
    (x',a') = runState (runReaderT (renamer x) stack) a