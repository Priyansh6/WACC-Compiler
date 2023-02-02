{-# LANGUAGE OverloadedStrings #-}

module Semantics.RenamerSpec (spec) where

import Data.Map as M
import Data.Text as T

import AST
import Renamer
import Test.Hspec

spec :: Spec
spec = do
  it "renames parameters" $
    renameFunc (ScopeAccum { vars = M.empty, scopeStack = [],  scopeCounter = 0}) 
               (Func WInt (Ident (pack "ashdkf")) [(WInt, Ident "x"), (WInt, Ident "y")] Skip) 
               `shouldBe` 
               (ScopeAccum {vars = fromList [(Ident "x0",Ident "x"),(Ident "y0",Ident "y")], scopeStack = [], scopeCounter = 0},
                Func WInt (Ident "ashdkf") [(WInt,Ident "x0"),(WInt,Ident "y0")] Skip)