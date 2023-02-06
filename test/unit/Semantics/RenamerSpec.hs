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
    renameFunc (ScopeAccum { 
                    scopeMap = M.empty, 
                    scopeStack = [0],  
                    scopeCounter = 0
                }) 
               (Func WInt (Ident (pack "ashdkf")) [(WInt, Ident "x"), (WInt, Ident "y")] Skip) 
               `shouldBe` 
               (ScopeAccum {
                   scopeMap = fromList [(0, [Ident "ashdkf"]), (1, [Ident "y-1", Ident "x-1"])],
                   scopeStack = [0],
                   scopeCounter = 1
                 }, Func WInt (Ident "ashdkf") [(WInt, Ident "x-1"), (WInt, Ident "y-1")] Skip)