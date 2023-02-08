{-# LANGUAGE OverloadedStrings #-}

module Semantics.RenamerSpec (spec) where

import Data.Map

import AST
import Renamer
import Test.Hspec

xScopeAccum :: ScopeAccum
xScopeAccum = initialScopeAccum {scopeMap = fromList [(0,[Ident "x-0"])]}

funcScopeAccum :: ScopeAccum
funcScopeAccum = initialScopeAccum {scopeMap = fromList [(0, [Ident "func"])]}

spec :: Spec
spec = do
  it "renames program functions" $
    renameProg initialScopeAccum (Program [Func WInt (Ident "func") [(WInt, Ident "x")] []] [])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "func"]),(1,[Ident "x-1"])], 
                scopeStack = [0], 
                scopeCounter = 1, 
                errors = []
                }, Program [Func WInt (Ident "func") [(WInt,Ident "x-1")] []] [])

  it "renames program body " $
    renameProg initialScopeAccum (Program [] [DecAssign WInt (Ident "x") (RExpr (IntLiter 1))])
              `shouldBe`
              (xScopeAccum, Program [] [DecAssign WInt (Ident "x-0") (RExpr (IntLiter 1))])


  it "renaming function adds it to scope map" $
    renameFunc initialScopeAccum (Func WInt (Ident "func") [] [])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "func"])], 
                scopeStack = [0], 
                scopeCounter = 1, 
                errors = []
              }, Func WInt (Ident "func") [] [])

  it "renames function parameters" $
    renameFunc initialScopeAccum (Func WInt (Ident "func") [(WInt, Ident "x"), (WInt, Ident "y")] []) 
              `shouldBe` 
              (ScopeAccum {
                scopeMap = fromList [(0, [Ident "func"]), (1, [Ident "y-1", Ident "x-1"])],
                scopeStack = [0],
                scopeCounter = 1,
                errors = []
              }, Func WInt (Ident "func") [(WInt, Ident "x-1"), (WInt, Ident "y-1")] [])
              
  it "renames function body" $
    renameFunc initialScopeAccum (Func WInt (Ident "func") [(WInt, Ident "x")] [DecAssign WInt (Ident "y") (RExpr (IdentExpr (Ident "x")))])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "func"]),(1,[Ident "y-1",Ident "x-1"])], 
                scopeStack = [0], 
                scopeCounter = 1, 
                errors = []
              }, Func WInt (Ident "func") [(WInt,Ident "x-1")] [DecAssign WInt (Ident "y-1") (RExpr (IdentExpr (Ident "x-1")))])

  it "can't rename already defined function" $
    renameFunc funcScopeAccum (Func WInt (Ident "func") [] [])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "func"])], 
                scopeStack = [0], 
                scopeCounter = 1, 
                errors = ["Error: Function func already defined."]
              }, Func WInt (Ident "func") [] [])

  it "renames parameters" $
    renameParam initialScopeAccum (WInt, Ident "x")
              `shouldBe`
              (xScopeAccum, (WInt,Ident "x-0"))
  
  it "renames skip statement" $
    renameStat initialScopeAccum Skip
              `shouldBe`
              (initialScopeAccum, Skip)
  
  it "renames declaration assignments" $
    renameStat initialScopeAccum (DecAssign WInt (Ident "x") (RExpr (IntLiter 1)))
              `shouldBe`
              (xScopeAccum, DecAssign WInt (Ident "x-0") (RExpr (IntLiter 1)))

  it "can't rename declaration assignments for variables already defined in same scope" $
    renameStat xScopeAccum (DecAssign WInt (Ident "x") (RExpr (IntLiter 1)))
              `shouldBe`
              (ScopeAccum {scopeMap = fromList [(0,[Ident "x-0"])], 
                scopeStack = [0], 
                scopeCounter = 0, 
                errors = ["Error: Variable x already defined."]
              }, DecAssign WInt (Ident "x") (RExpr (IntLiter 1)))
  
  it "renames declaration assignments for variables defined in other scope" $
    renameStat (xScopeAccum {scopeStack = [1, 0]}) (DecAssign WInt (Ident "x") (RExpr (IntLiter 1)))
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "x-0"]),(1,[Ident "x-1"])], 
                scopeStack = [1,0], 
                scopeCounter = 0, 
                errors = []
              }, DecAssign WInt (Ident "x-1") (RExpr (IntLiter 1)))
  
  it "renames non-declaration assignments" $
    renameStat xScopeAccum (Assign (LIdent (Ident "x")) (RExpr (IntLiter 1)))
              `shouldBe`
              (xScopeAccum, Assign (LIdent (Ident "x-0")) (RExpr (IntLiter 1)))
  
  it "renames non-declaration assignments in child scopes" $
    renameStat (xScopeAccum {scopeStack = [1,0]}) (Assign (LIdent (Ident "x")) (RExpr (IntLiter 1)))
              `shouldBe`
              (xScopeAccum {scopeStack = [1,0]}, Assign (LIdent (Ident "x-0")) (RExpr (IntLiter 1)))
  
  it "can't rename non-declaration assignments if variable is not defined in scope" $
    renameStat initialScopeAccum (Assign (LIdent (Ident "x")) (RExpr (IntLiter 1)))
              `shouldBe`
              (initialScopeAccum {errors = ["Error: Variable x not defined."]}, Assign (LIdent (Ident "x")) (RExpr (IntLiter 1)))
  
  it "renames read statements" $
    renameStat xScopeAccum (Read (LIdent (Ident "x")))
              `shouldBe`
              (xScopeAccum, Read (LIdent (Ident "x-0")))
  
  it "renames free statements" $
    renameStat xScopeAccum (Free (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, Free (IdentExpr (Ident "x-0")))
   
  it "renames return statements" $
    renameStat xScopeAccum (Return (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, Return (IdentExpr (Ident "x-0")))
   
  it "renames exit statements" $
    renameStat xScopeAccum (Exit (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, Exit (IdentExpr (Ident "x-0")))
   
  it "renames print statements" $
    renameStat xScopeAccum (Print (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, Print (IdentExpr (Ident "x-0")))
   
  it "renames println statements" $
    renameStat xScopeAccum (Println (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, Println (IdentExpr (Ident "x-0")))
  
  it "renames if statements" $
    renameStat xScopeAccum (If (IdentExpr (Ident "x")) [DecAssign WInt (Ident "x") (RExpr (IntLiter 1))] [DecAssign WInt (Ident "x") (RExpr (IntLiter 1))])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "x-0"]),(1,[Ident "x-1"]),(2,[Ident "x-2"])], 
                scopeStack = [0], 
                scopeCounter = 2, 
                errors = []
              }, If (IdentExpr (Ident "x-0")) [DecAssign WInt (Ident "x-1") (RExpr (IntLiter 1))] [DecAssign WInt (Ident "x-2") (RExpr (IntLiter 1))])

  it "renames while statements" $
    renameStat xScopeAccum (While (IdentExpr (Ident "x")) [DecAssign WInt (Ident "x") (RExpr (IntLiter 1))])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "x-0"]),(1,[Ident "x-1"])], 
                scopeStack = [0], 
                scopeCounter = 1, 
                errors = []
              }, While (IdentExpr (Ident "x-0")) [DecAssign WInt (Ident "x-1") (RExpr (IntLiter 1))])

  it "renames begin statements" $
    renameStat initialScopeAccum (Begin [DecAssign WInt (Ident "x") (RExpr (IntLiter 1))])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(1,[Ident "x-1"])], 
                scopeStack = [0], 
                scopeCounter = 1, 
                errors = []
                }, Begin [DecAssign WInt (Ident "x-1") (RExpr (IntLiter 1))])
  
  it "renames lidents" $
    renameLVal xScopeAccum (LIdent (Ident "x"))
              `shouldBe`
              (xScopeAccum, (LIdent (Ident "x-0")))
  
  it "renames larrays" $
    renameLVal xScopeAccum (LArray (ArrayElem (Ident "x") [IdentExpr (Ident "x")]))
              `shouldBe`
              (xScopeAccum, LArray (ArrayElem (Ident "x-0") [IdentExpr (Ident "x-0")]))
  
  it "renames lpairs" $
    renameLVal xScopeAccum (LPair (Fst (LIdent (Ident "x"))))
              `shouldBe`
              (xScopeAccum, LPair (Fst (LIdent (Ident "x-0"))))
  
  it "renames rexprs" $
    renameRVal xScopeAccum (RExpr (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, RExpr (IdentExpr (Ident "x-0")))

  it "renames arrayliters" $
    renameRVal xScopeAccum (ArrayLiter [IdentExpr (Ident "x")])
              `shouldBe`
              (xScopeAccum, ArrayLiter [IdentExpr (Ident "x-0")])
  
  it "renames newpairs" $
    renameRVal xScopeAccum (NewPair (IdentExpr (Ident "x")) (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, NewPair (IdentExpr (Ident "x-0")) (IdentExpr (Ident "x-0")))

  it "renames rpairs" $
    renameRVal xScopeAccum (RPair (Fst (LIdent (Ident "x"))))
              `shouldBe`
              (xScopeAccum, RPair (Fst (LIdent (Ident "x-0"))))
  
  it "renames calls" $
    renameRVal (initialScopeAccum {scopeMap = fromList [(0, [(Ident "func"), (Ident "x-0")])]}) (Call (Ident "func") [IdentExpr (Ident "x")])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "func",Ident "x-0"])], 
                scopeStack = [0], 
                scopeCounter = 0, 
                errors = []
              }, Call (Ident "func") [IdentExpr (Ident "x-0")])
  
  it "can't rename function calls if function doesn't exist" $
    renameRVal xScopeAccum (Call (Ident "func") [IdentExpr (Ident "x")])
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "x-0"])], 
                scopeStack = [0], 
                scopeCounter = 0, 
                errors = ["Error: Function func not defined."]
              }, Call (Ident "func") [IdentExpr (Ident "x-0")])
  
  it "renames identexprs" $
    renameExpr xScopeAccum (IdentExpr (Ident "x"))
              `shouldBe`
              (xScopeAccum, IdentExpr (Ident "x-0"))
  
  it "renames arrayexprs" $
    renameExpr xScopeAccum (ArrayExpr (ArrayElem (Ident "x") [IdentExpr (Ident "x")]))
              `shouldBe`
              (xScopeAccum, ArrayExpr (ArrayElem (Ident "x-0") [IdentExpr (Ident "x-0")]))
  
  it "renames unary operator expressions" $
    renameExpr xScopeAccum (Len (IdentExpr (Ident "x")))
              `shouldBe`
              (xScopeAccum, Len (IdentExpr (Ident "x-0")))

  it "renames binary operator expressions" $
    renameExpr xScopeAccum (IdentExpr (Ident "x") :*: IdentExpr (Ident "x"))
              `shouldBe`
              (xScopeAccum, IdentExpr (Ident "x-0") :*: IdentExpr (Ident "x-0"))
  
  it "renames array elems" $
    renameArrayElem xScopeAccum (ArrayElem (Ident "x") [IdentExpr (Ident "x")])
              `shouldBe`
              (xScopeAccum, ArrayElem (Ident "x-0") [IdentExpr (Ident "x-0")])
  
  it "renames fst pair elems" $
    renamePairElem xScopeAccum (Fst (LIdent (Ident "x")))
              `shouldBe`
              (xScopeAccum, Fst (LIdent (Ident "x-0")))
  
  it "renames snd pair elems" $
    renamePairElem xScopeAccum (Snd (LIdent (Ident "x")))
              `shouldBe`
              (xScopeAccum, Snd (LIdent (Ident "x-0")))
  
  it "renames and updates scope map for undeclared idents" $
    renameUndeclaredIdent initialScopeAccum (Ident "x")
              `shouldBe`
              (initialScopeAccum {scopeMap = fromList [(0,[Ident "x-0"])]}, Ident "x-0")

  it "can't rename undeclared ident if it has already been declared in the current scope" $
    renameUndeclaredIdent xScopeAccum (Ident "x")
              `shouldBe`
              (xScopeAccum {errors = ["Error: Variable x already defined."]}, Ident "x")

  it "renames declared idents" $
    renameDeclaredIdent xScopeAccum (Ident "x")
              `shouldBe`
              (xScopeAccum, (Ident "x-0"))
  
  it "renames declared idents in a child scope" $
    renameDeclaredIdent (xScopeAccum {scopeStack = [2,1,0]}) (Ident "x")
              `shouldBe`
              (xScopeAccum {scopeStack = [2,1,0]}, (Ident "x-0"))
        
  it "can't rename declared ident if it hasn't been defined in current scope" $
    renameDeclaredIdent initialScopeAccum (Ident "x")
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [], 
                scopeStack = [0], 
                scopeCounter = 0, 
                errors = ["Error: Variable x not defined."]
              }, Ident "x")
  
  it "can't rename declared ident if it hasn't been declared in a parent scope" $
    renameDeclaredIdent (xScopeAccum {scopeStack = [3,2,1]}) (Ident "x")
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "x-0"])], 
                scopeStack = [3,2,1], 
                scopeCounter = 0, 
                errors = ["Error: Variable x not defined."]
              }, Ident "x")
  
  it "renames declared ident with closest possible parent scope" $
    renameDeclaredIdent (initialScopeAccum {scopeMap = fromList [(0,[Ident "x-0"]), (1,[Ident "x-1"])], scopeStack = [2,1,0]}) (Ident "x")
              `shouldBe`
              (ScopeAccum {
                scopeMap = fromList [(0,[Ident "x-0"]),(1,[Ident "x-1"])], 
                scopeStack = [2,1,0], 
                scopeCounter = 0, 
                errors = []
              }, Ident "x-1")