{-# LANGUAGE OverloadedStrings #-}

module UnitTest.Semantic.RenamerSpec (spec) where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.Hspec

import AST
import Semantic.Errors
import Semantic.Rename.Utils
import Semantic.Rename.Function
import Semantic.Rename.Program
import Semantic.Rename.RLValExpr
import Semantic.Rename.Statement
import UnitTest.Semantic.Test

xAux :: Aux
xAux = initAux {scopeMap = M.fromList [(0, S.fromList [Ident "x-0" (0, 0)])]}

doubleXAux :: Aux
doubleXAux = initAux {scopeMap = M.fromList [(0, S.fromList [Ident "x-0" (0, 0)]), (1, S.fromList [Ident "x-1" (0, 0)])]}

xFuncAux :: Aux
xFuncAux = initAux {funcSet = S.fromList [(Ident "func?int" (0, 0))], scopeMap = M.fromList [(0, S.fromList [Ident "x-0" (0, 0)])]}

spec :: Spec
spec = do
  it "renames program functions" $
    test initAux renameProg (Program [Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x" (0, 0))] [] Nothing (0, 0)] [])
      `shouldBe` Right (Program [Func WInt (Ident "func.ii" (0, 0)) [(WInt, Ident "x-1" (0, 0))] [] (Just 2) (0, 0)] [])

  it "renames program body " $
    test initAux renameProg (Program [] [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)])
      `shouldBe` Right (Program [] [DecAssign WInt (Ident "x-0" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)])

  it "renames function parameters" $
    test initAux renameFunc (Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x" (0, 0)), (WInt, Ident "y" (0, 0))] [] Nothing (0, 0))
      `shouldBe` Right (Func WInt (Ident "func.iii" (0, 0)) [(WInt, Ident "x-1" (0, 0)), (WInt, Ident "y-1" (0, 0))] [] (Just 2) (0, 0))

  it "renames function body" $
    test initAux renameFunc (Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x" (0, 0))] [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IdentExpr (Ident "x" (0, 0)) (0, 0))) (0, 0)] Nothing (0, 0))
      `shouldBe` Right (Func WInt (Ident "func.ii" (0, 0)) [(WInt, Ident "x-1" (0, 0))] [DecAssign WInt (Ident "x-2" (0, 0)) (RExpr (IdentExpr (Ident "x-1" (0, 0)) (0, 0))) (0, 0)] (Just 2) (0, 0))

  it "renames overloaded functions" $
    test initAux renameProg (Program [Func WInt (Ident "func" (0, 0)) [(WInt, (Ident "x" (0, 0)))] [] Nothing (0, 0), Func WInt (Ident "func" (0, 0)) [(WBool, (Ident "x" (0, 0)))] [] Nothing (0, 0)] [])
      `shouldBe` Right (Program [Func WInt (Ident "func.ii" (0, 0)) [(WInt,Ident "x-1" (0, 0))] [] (Just 2) (0, 0),Func WInt (Ident "func.ib" (0, 0)) [(WBool,Ident "x-3" (0, 0))] [] (Just 4) (0, 0)] [])

  it "can't rename already defined function" $
    test initAux renameProg (Program [Func WInt (Ident "func" (0, 0)) [] [] Nothing (0, 0), Func WInt (Ident "func" (0, 0)) [] [] Nothing (0, 0)] [])
      `shouldBe` Left [FunctionAlreadyDefined (Ident "func" (0, 0)) WInt []]
  
  it "renames functions that are mutually recursive" $
    test initAux renameProg (Program [Func WInt (Ident "func1" (0, 0)) [] [DecAssign WInt (Ident "x" (0, 0)) (Call (Ident "func2" (0, 0)) [] (0, 0)) (0, 0)] Nothing (0, 0), Func WInt (Ident "func2" (0,0)) [] [DecAssign WInt (Ident "x" (0,0)) (Call (Ident "func1" (0,0)) [] (0,0)) (0,0)] Nothing (0,0)] [])
      `shouldBe` Right (Program [Func WInt (Ident "func1.i" (0,0)) [] [DecAssign WInt (Ident "x-2" (0,0)) (Call (Ident "func2" (0,0)) [] (0,0)) (0,0)] (Just 2) (0,0),Func WInt (Ident "func2.i" (0,0)) [] [DecAssign WInt (Ident "x-4" (0,0)) (Call (Ident "func1" (0,0)) [] (0,0)) (0,0)] (Just 4) (0,0)] [])

  it "renames parameters" $
    test initAux renameParam (WInt, Ident "x" (0, 0))
      `shouldBe` Right (WInt, Ident "x-0" (0, 0))

  it "renames skip statement" $
    test initAux renameStat Skip
      `shouldBe` Right Skip

  it "renames declaration assignments" $
    test initAux renameStat (DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` Right (DecAssign WInt (Ident "x-0" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0))

  it "can't rename declaration assignments for variables already defined in same scope" $
    test xAux renameStat (DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` Left [VariableAlreadyDefined (Ident "x" (0, 0))]

  it "renames declaration assignments for variables defined in other scope" $
    testWithStack [1, 0] xAux renameStat (DecAssign WInt (Ident "x" (0, 0)) (RExpr (IdentExpr (Ident "x" (0, 0)) (0, 0))) (0, 0))
      `shouldBe` Right (DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IdentExpr (Ident "x-0" (0, 0)) (0, 0))) (0, 0))

  it "renames non-declaration assignments" $
    test xAux renameStat (Assign (LIdent (Ident "x" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` Right (Assign (LIdent (Ident "x-0" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))

  it "renames non-declaration assignments in child scopes" $
    testWithStack [1, 0] xAux renameStat (Assign (LIdent (Ident "x" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` Right (Assign (LIdent (Ident "x-0" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))

  it "can't rename non-declaration assignments if variable is not defined in scope" $
    test initAux renameStat (Assign (LIdent (Ident "x" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` Left [VariableNotDefined (Ident "x" (0, 0))]

  it "renames read statements" $
    test xAux renameStat (Read (LIdent (Ident "x" (0, 0))) (0, 0))
      `shouldBe` Right (Read (LIdent (Ident "x-0" (0, 0))) (0, 0))

  it "renames free statements" $
    test xAux renameStat (Free (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` Right (Free (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames return statements" $
    test xAux renameStat (Return (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` Right (Return (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames exit statements" $
    test xAux renameStat (Exit (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` Right (Exit (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames print statements" $
    test xAux renameStat (Print (IdentExpr (Ident "x" (0, 0)) (0, 0)))
      `shouldBe` Right (Print (IdentExpr (Ident "x-0" (0, 0)) (0, 0)))

  it "renames println statements" $
    test xAux renameStat (Println (IdentExpr (Ident "x" (0, 0)) (0, 0)))
      `shouldBe` Right (Println (IdentExpr (Ident "x-0" (0, 0)) (0, 0)))

  it "renames if statements" $
    test xAux renameStat (If (IdentExpr (Ident "x" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] Nothing [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] Nothing (0, 0))
      `shouldBe` Right (If (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (Just 1) [DecAssign WInt (Ident "x-2" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (Just 2) (0, 0))

  it "renames while statements" $
    test xAux renameStat (While (IdentExpr (Ident "x" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] Nothing (0, 0))
      `shouldBe` Right (While (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (Just 1) (0, 0))

  it "renames begin statements" $
    test initAux renameStat (Begin [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] Nothing)
      `shouldBe` Right (Begin [DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (Just 1))

  it "renames lidents" $
    test xAux renameLVal (LIdent (Ident "x" (0, 0)))
      `shouldBe` Right (LIdent (Ident "x-0" (0, 0)))

  it "renames larrays" $
    test xAux renameLVal (LArray (ArrayElem (Ident "x" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0)))
      `shouldBe` Right (LArray (ArrayElem (Ident "x-0" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0)))

  it "renames lpairs" $
    test xAux renameLVal (LPair (Fst (LIdent (Ident "x" (0, 0))) (0, 0)))
      `shouldBe` Right (LPair (Fst (LIdent (Ident "x-0" (0, 0))) (0, 0)))

  it "renames rexprs" $
    test xAux renameRVal (RExpr (IdentExpr (Ident "x" (0, 0)) (0, 0)))
      `shouldBe` Right (RExpr (IdentExpr (Ident "x-0" (0, 0)) (0, 0)))

  it "renames arrayliters" $
    test xAux renameRVal (ArrayLiter [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0))
      `shouldBe` Right (ArrayLiter [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0))

  it "renames newpairs" $
    test xAux renameRVal (NewPair (IdentExpr (Ident "x" (0, 0)) (0, 0)) (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` Right (NewPair (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames rpairs" $
    test xAux renameRVal (RPair (Fst (LIdent (Ident "x" (0, 0))) (0, 0)))
      `shouldBe` Right (RPair (Fst (LIdent (Ident "x-0" (0, 0))) (0, 0)))

  it "renames calls" $
    test xFuncAux renameRVal (Call (Ident "func" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0))
      `shouldBe` Right (Call (Ident "func" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0))

  it "renames identexprs" $
    test xAux renameExpr (IdentExpr (Ident "x" (0, 0)) (0, 0))
      `shouldBe` Right (IdentExpr (Ident "x-0" (0, 0)) (0, 0))

  it "renames arrayexprs" $
    test xAux renameExpr (ArrayExpr (ArrayElem (Ident "x" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0)) (0, 0))
      `shouldBe` Right (ArrayExpr (ArrayElem (Ident "x-0" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0)) (0, 0))

  it "renames unary operator expressions" $
    test xAux renameExpr (Len (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` Right (Len (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames binary operator expressions" $
    test xAux renameExpr ((:*:) (IdentExpr (Ident "x" (0, 0)) (0, 0)) (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` Right ((:*:) (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames array elems" $
    test xAux renameArrayElem (ArrayElem (Ident "x" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0))
      `shouldBe` Right (ArrayElem (Ident "x-0" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0))

  it "renames fst pair elems" $
    test xAux renamePairElem (Fst (LIdent (Ident "x" (0, 0))) (0, 0))
      `shouldBe` Right (Fst (LIdent (Ident "x-0" (0, 0))) (0, 0))

  it "renames snd pair elems" $
    test xAux renamePairElem (Snd (LIdent (Ident "x" (0, 0))) (0, 0))
      `shouldBe` Right (Snd (LIdent (Ident "x-0" (0, 0))) (0, 0))

  it "renames undeclared idents" $
    test initAux renameUndeclaredIdent (Ident "x" (0, 0))
      `shouldBe` Right (Ident "x-0" (0, 0))

  it "can't rename undeclared ident if it has already been declared in the current scope" $
    test xAux renameUndeclaredIdent (Ident "x" (0, 0))
      `shouldBe` Left [VariableAlreadyDefined (Ident "x" (0, 0))]
  
  it "renames declared idents" $
    test xAux renameDeclaredIdent (Ident "x" (0, 0))
      `shouldBe` Right (Ident "x-0" (0, 0))

  it "renames declared idents in a child scope" $
    testWithStack [2, 1, 0] xAux renameDeclaredIdent (Ident "x" (0, 0))
      `shouldBe` Right (Ident "x-0" (0, 0))

  it "can't rename declared ident if it hasn't been defined in current scope" $
    test initAux renameDeclaredIdent (Ident "x" (0, 0))
      `shouldBe` Left [VariableNotDefined (Ident "x" (0, 0))]

  it "can't rename declared ident if it hasn't been declared in a parent scope" $
    testWithStack [3, 2, 1] xAux renameDeclaredIdent (Ident "x" (0, 0))
      `shouldBe` Left [VariableNotDefined (Ident "x" (0, 0))]

  it "renames declared ident with closest possible parent scope" $
    testWithStack [2, 1, 0] doubleXAux renameDeclaredIdent  (Ident "x" (0, 0))
      `shouldBe` Right (Ident "x-1" (0, 0))