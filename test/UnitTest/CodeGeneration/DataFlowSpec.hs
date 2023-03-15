{-# LANGUAGE OverloadedStrings #-}

module UnitTest.CodeGeneration.DataFlowSpec (spec) where

import CodeGeneration.Intermediate.ControlFlow 
import CodeGeneration.Intermediate.DataFlow 
import CodeGeneration.Intermediate.IR
import Data.Map ((!))
import Test.Hspec

import qualified Data.Map as M
import qualified Data.Set as Set

-- Read as a dominates b
dominates :: Id -> Id -> CFG a -> Bool
dominates a b CFG {nodes = ns}
  = Set.member a (dominators (ns ! b))

-- Read as a reverseDominates b
reverseDominates :: Id -> Id -> CFG a -> Bool
reverseDominates a b CFG {nodes = ns}
  = Set.member a (revDominators (ns ! b))

spec :: Spec
spec = do
  -- Dominator Tests
  let is1 = [ Mov (Reg (TmpReg 1)) (Imm 8)
            , Define "label" 
            , Jmp "label" ]
      cfg1 = toCFG is1
      cfg1' = solveDominatorDFE cfg1
  it "non jump instruction nodes dominate themselves" $ 
    dominates (Right 0) (Right 0) cfg1' `shouldBe` True

  it "jump nodes dominate themselves" $
    dominates (Right 2) (Right 2) cfg1' `shouldBe` True

  it "label nodes dominate themselves" $
    dominates (Left "label") (Left "label") cfg1' `shouldBe` True

  let is2 = [ Mov (Reg (TmpReg 1)) (Imm 8)
            , Mov (Reg (TmpReg 2)) (Imm 8)
            , Add (Reg (TmpReg 1)) (Reg (TmpReg 2)) (Reg (TmpReg 1)) ]
      cfg2 = toCFG is2
      cfg2' = solveDominatorDFE cfg2
  it "linear instructions dominate sequentially" $
    dominates (Right 0) (Right 1) cfg2' &&
    dominates (Right 0) (Right 2) cfg2' &&
    dominates (Right 1) (Right 2) cfg2' `shouldBe` True

  let is3 = [ Mov (Reg (TmpReg 1)) (Imm 8)
            , Jmp "end"
            , Define "end" ]
      cfg3 = toCFG is3
      cfg3' = solveDominatorDFE cfg3
  it "unconditional jump instructions dominate sequentially" $
    dominates (Right 0) (Right 1) cfg3' &&
    dominates (Right 0) (Left "end") cfg3' &&
    dominates (Right 1) (Left "end") cfg3' `shouldBe` True

  let is4 = [ Mov (Reg (TmpReg 1)) (Imm 8) -- Right 0
            , Cmp (Reg (TmpReg 1)) (Imm 5) -- Right 1
            , Jl "branch1"                 -- Right 2
            , Mov (Reg (TmpReg 0)) (Imm 1) -- Right 3
            , Jmp "end"                    -- Right 4
            , Define "branch1"             -- Left "branch1"
            , Mov (Reg (TmpReg 0)) (Imm 0) -- Right 6
            , Define "end" ]               -- Left "end"
      cfg4 = toCFG is4
      cfg4' = solveDominatorDFE cfg4
  it "instructions before a branch dominate all nodes on both branches" $
    [ dominates (Right 0) (Right 2) cfg4' 
    , dominates (Right 0) (Right 3) cfg4'
    , dominates (Right 0) (Right 4) cfg4'
    , dominates (Right 0) (Left "branch1") cfg4' 
    , dominates (Right 0) (Right 6) cfg4' 
    , dominates (Right 0) (Left "end") cfg4' 
    , dominates (Right 1) (Right 2) cfg4'
    , dominates (Right 1) (Right 3) cfg4'
    , dominates (Right 1) (Right 4) cfg4'
    , dominates (Right 1) (Left "branch1") cfg4' 
    , dominates (Right 1) (Right 6) cfg4' 
    , dominates (Right 1) (Left "end") cfg4' 
    , dominates (Right 2) (Right 2) cfg4'
    , dominates (Right 2) (Right 3) cfg4'
    , dominates (Right 2) (Right 4) cfg4'
    , dominates (Right 2) (Left "branch1") cfg4' 
    , dominates (Right 2) (Right 6) cfg4' 
    , dominates (Right 2) (Left "end") cfg4' ] `shouldSatisfy` and 

  it "instructions on different branches don't dominate the exit node" $
    [ dominates (Right 3) (Left "end") cfg4' 
    , dominates (Right 4) (Left "end") cfg4'
    , dominates (Left "branch1") (Left "end") cfg4' 
    , dominates (Right 6) (Left "end") cfg4' ] `shouldSatisfy` (not . or)

  -- Reverse Dominator Tests
  let is5 = [ Mov (Reg (TmpReg 1)) (Imm 8)
            , Define "label" 
            , Jmp "label" ]
      cfg5 = toCFG is5
      cfg5' = solveRevDominatorDFE cfg5
  it "non jump instruction nodes reverse dominate themselves" $ 
    reverseDominates (Right 0) (Right 0) cfg5' `shouldBe` True

  it "jump nodes dominate themselves" $
    reverseDominates (Right 2) (Right 2) cfg5' `shouldBe` True

  it "label nodes dominate themselves" $
    reverseDominates (Left "label") (Left "label") cfg5' `shouldBe` True

  let is6 = [ Mov (Reg (TmpReg 1)) (Imm 8)
            , Mov (Reg (TmpReg 2)) (Imm 8)
            , Add (Reg (TmpReg 1)) (Reg (TmpReg 2)) (Reg (TmpReg 1)) ]
      cfg6 = toCFG is6
      cfg6' = solveRevDominatorDFE cfg6
  it "linear instructions reverse-dominate sequentially" $
    reverseDominates (Right 1) (Right 0) cfg6' &&
    reverseDominates (Right 2) (Right 0) cfg6' &&
    reverseDominates (Right 2) (Right 1) cfg6' `shouldBe` True

  let is7 = [ Mov (Reg (TmpReg 1)) (Imm 8)
            , Jmp "end"
            , Define "end" ]
      cfg7 = toCFG is7
      cfg7' = solveRevDominatorDFE cfg7
  it "unconditional jump instructions reverse-dominate sequentially" $
    reverseDominates (Right 1) (Right 0) cfg7' &&
    reverseDominates (Left "end") (Right 0) cfg7' &&
    reverseDominates (Left "end") (Right 1) cfg7' `shouldBe` True

  let is8 = [ Mov (Reg (TmpReg 1)) (Imm 8) -- Right 0
            , Cmp (Reg (TmpReg 1)) (Imm 5) -- Right 1
            , Jl "branch1"                 -- Right 2
            , Mov (Reg (TmpReg 0)) (Imm 1) -- Right 3
            , Jmp "end"                    -- Right 4
            , Define "branch1"             -- Left "branch1"
            , Mov (Reg (TmpReg 0)) (Imm 0) -- Right 6
            , Define "end" ]               -- Left "end"
      cfg8 = toCFG is8
      cfg8' = solveRevDominatorDFE cfg8
  it "instructions after a branch reverse-dominate all nodes on both branches" $
    [ reverseDominates (Left "end") (Right 0) cfg8' 
    , reverseDominates (Left "end") (Right 1) cfg8'
    , reverseDominates (Left "end") (Right 2) cfg8'
    , reverseDominates (Left "end") (Right 3) cfg8' 
    , reverseDominates (Left "end") (Right 4) cfg8' 
    , reverseDominates (Left "end") (Left "branch1") cfg8' 
    , reverseDominates (Left "end") (Right 6) cfg8'
    , reverseDominates (Right 6) (Left "branch1") cfg8' 
    , reverseDominates (Right 4) (Right 3) cfg8' 
    , reverseDominates (Right 2) (Right 0) cfg8' 
    , reverseDominates (Right 2) (Right 1) cfg8' ] `shouldSatisfy` and

  it "instructions on different branches don't reverse-dominate the start node" $
    [ reverseDominates (Right 3) (Right 0) cfg8' 
    , reverseDominates (Right 4) (Right 0) cfg8'
    , reverseDominates (Left "branch1") (Right 0) cfg8' 
    , reverseDominates (Right 6) (Right 0) cfg8' ] `shouldSatisfy` (not . or)
