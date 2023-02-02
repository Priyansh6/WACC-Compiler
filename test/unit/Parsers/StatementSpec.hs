{-# LANGUAGE OverloadedStrings #-}

module Parsers.StatementSpec (spec) where

import AST
import Statements
import Parsers.Test
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  it "skip" $ do
    test pSkip "skip" `shouldParse` Skip 
