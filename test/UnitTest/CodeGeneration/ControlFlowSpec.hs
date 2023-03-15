{-# LANGUAGE OverloadedStrings #-}

module UnitTest.CodeGeneration.ControlFlowSpec (spec) where

import CodeGeneration.Intermediate.ControlFlow 
import CodeGeneration.Intermediate.IR
import Data.Map ((!))
import Data.List (sort)
import Test.Hspec

import qualified Data.Map as M
import qualified Data.Set as Set

spec :: Spec
spec = do
  let is1 = [Mov (Reg (TmpReg 1)) (Imm 8)]
      cfg1 = toCFG is1
  it "constructs a singleton node for a body of one instruction" $ 
    (nodes cfg1) ! (Right 0) `shouldBe` mkCFGNode (Right 0) (Mov (Reg (TmpReg 1)) (Imm 8)) True
  it "adds no edges for a body of one instruction" $ 
    edges cfg1 `shouldBe` Set.empty

  let is2 = [Define "label"] :: IRInstrs
      cfg2 = toCFG is2
  it "constructs nodes with a label id for define instructions" $ 
    (nodes cfg2) ! (Left "label") `shouldBe` mkCFGNode (Left "label") (Define "label") True

  let is3 = [ Mov (Reg (TmpReg 1)) (Imm 8)
            , Mov (Reg (TmpReg 2)) (Imm 7) 
            , Add (Reg (TmpReg 1)) (Reg (TmpReg 2)) (Reg (TmpReg 1)) ]
      cfg3 = toCFG is3
  it "constructs multiple nodes for a body of multiple instructions" $ 
    nodes cfg3 `shouldBe` M.fromList [ (Right 0, mkCFGNode (Right 0) (Mov (Reg (TmpReg 1)) (Imm 8)) True) 
                                     , (Right 1, mkCFGNode (Right 1) (Mov (Reg (TmpReg 2)) (Imm 7)) False)
                                     , (Right 2, mkCFGNode (Right 2) (Add (Reg (TmpReg 1)) (Reg (TmpReg 2)) (Reg (TmpReg 1))) False) ]
  it "constructs nodes pointing in sequence for a linear control flow" $ 
    edges cfg3 `shouldBe` Set.fromList [(Right 0,Right 1),(Right 1,Right 2)]

  let is4 = [ Mov (Reg (TmpReg 0)) (Imm 5)
            , Cmp (Reg (TmpReg 0)) (Imm 1)
            , Jmp "equal"
            , Mov (Reg (TmpReg 1)) (Imm 1)
            , Jmp "exit"
            , Define "equal"
            , Mov (Reg (TmpReg 1)) (Imm 0)
            , Jmp "exit"
            , Define "exit" ]
      cfg4 = toCFG is4

  it "correctly constructs edges in the case of a jump instruction" $
    edges cfg4 `shouldBe` Set.fromList [ (Right 0, Right 1)
                                       , (Right 1, Right 2)
                                       , (Right 2, Left "equal")
                                       , (Right 3, Right 4)
                                       , (Right 4, Left "exit")
                                       , (Left "equal", Right 6)
                                       , (Right 6, Right 7)
                                       , (Right 7, Left "exit")]

  it "correctly identifies the successors of unconditional jump nodes" $
    successors (Right 2) cfg4 `shouldBe` [Left "equal"]

  let is5 = [ Mov (Reg (TmpReg 0)) (Imm 5) -- Right 0
            , Cmp (Reg (TmpReg 0)) (Imm 1) -- Right 1
            , Je "equal"                   -- Right 2
            , Mov (Reg (TmpReg 1)) (Imm 1) -- Right 3
            , Jl "exit"                    -- Right 4
            , Define "equal"               -- Left "equal"
            , Mov (Reg (TmpReg 1)) (Imm 0) -- Right 6
            , Define "exit" ]              -- Left "exit"
      cfg5 = toCFG is5

  it "correctly constructs edges in the case of a conditional jump instruction" $
    edges cfg5 `shouldBe` Set.fromList [ (Right 0, Right 1)
                                       , (Right 1, Right 2)
                                       , (Right 2, Left "equal")
                                       , (Right 2, Right 3)
                                       , (Right 3, Right 4)
                                       , (Right 4, Left "exit")
                                       , (Right 4, Left "equal")
                                       , (Left "equal", Right 6)
                                       , (Right 6, Left "exit")]

  it "correctly identifies the successors of non jump nodes" $
    successors (Right 0) cfg5 `shouldBe` [Right 1]

  it "correctly identifies the successors of conditional jump nodes" $
    successors (Right 2) cfg5 `shouldBe` sort [Left "equal", Right 3]

