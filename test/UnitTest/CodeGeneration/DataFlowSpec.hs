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

spec :: Spec
spec = do
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
