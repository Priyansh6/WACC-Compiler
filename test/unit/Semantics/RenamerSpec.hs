{-# LANGUAGE OverloadedStrings #-}

module Semantics.RenamerSpec (spec) where

import Data.Map as M

import AST
import Renamer
import Test.Hspec

spec :: Spec
spec = do
  it "renames function parameters" $
    renameFunc initialScopeAccum (Func WInt (Ident "ashdkf") [(WInt, Ident "x"), (WInt, Ident "y")] [Skip]) 
               `shouldBe` 
               (ScopeAccum {
                   scopeMap = fromList [(0, [Ident "ashdkf"]), (1, [Ident "y-1", Ident "x-1"])],
                   scopeStack = [0],
                   scopeCounter = 1,
                   errors = []
                 }, Func WInt (Ident "ashdkf") [(WInt, Ident "x-1"), (WInt, Ident "y-1")] [Skip])