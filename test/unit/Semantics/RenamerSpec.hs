{-# LANGUAGE OverloadedStrings #-}

module Semantics.RenamerSpec (spec) where

import AST
import Data.Map
import Renamer
import RenameFunc
import RenameStat
import RenameRLValExpr
import Test.Hspec
import SemanticErrors
import Scope

xScopeAccum :: ScopeAccum
xScopeAccum = initialScopeAccum {scopeMap = fromList [(0, [Ident "x-0" (0, 0)])]}

funcScopeAccum :: ScopeAccum
funcScopeAccum = initialScopeAccum {scopeMap = fromList [(0, [Ident "func" (0, 0)])]}

spec :: Spec
spec = do
  it "renames program functions" $
    renameProg initialScopeAccum (Program [Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x" (0, 0))] [] (0, 0)] [])
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "func" (0, 0)]), (1, [Ident "x-1" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 2,
                       errors = []
                     },
                   Program [Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x-1" (0, 0))] [] (0, 0)] [] 
                 )

  it "renames program body " $
    renameProg initialScopeAccum (Program [] [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)])
      `shouldBe` (xScopeAccum, Program [] [DecAssign WInt (Ident "x-0" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)])

  it "renaming program adds functions to scope map" $
    renameProg initialScopeAccum (Program [Func WInt (Ident "func" (0, 0)) [] [] (0, 0)] [])
      `shouldBe` ( ScopeAccum 
                     { scopeMap = fromList [(0,[Ident "func" (0,0)])], 
                       scopeStack = [0], 
                       scopeCounter = 2, 
                       errors = []
                     },
                   Program [Func WInt (Ident "func" (0,0)) [] [] (0,0)] []
                 )

  it "renames function parameters" $
    renameFunc initialScopeAccum (Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x" (0, 0)), (WInt, Ident "y" (0, 0))] [] (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(1, [Ident "y-1" (0, 0), Ident "x-1" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 2,
                       errors = []
                     },
                   Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x-1" (0, 0)), (WInt, Ident "y-1" (0, 0))] [] (0, 0)
                 )

  it "renames function body" $
    renameFunc initialScopeAccum (Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x" (0, 0))] [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IdentExpr (Ident "x" (0, 0)) (0, 0))) (0, 0)] (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(1, [Ident "x-1" (0, 0)]), (2, [Ident "x-2" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 2,
                       errors = []
                     },
                   Func WInt (Ident "func" (0, 0)) [(WInt, Ident "x-1" (0, 0))] [DecAssign WInt (Ident "x-2" (0, 0)) (RExpr (IdentExpr (Ident "x-1" (0, 0)) (0, 0))) (0, 0)] (0, 0)
                 )

  it "can't rename already defined function" $
    renameProg funcScopeAccum (Program [Func WInt (Ident "func" (0, 0)) [] [] (0, 0)] [])
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "func" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 2,
                       errors = [FunctionAlreadyDefined (Ident "func" (0, 0))]
                     },
                   Program [Func WInt (Ident "func" (0, 0)) [] [] (0, 0)] []
                 )
  
  it "renames functions that are mutually recursive" $
    renameProg initialScopeAccum (Program [Func WInt (Ident "func1" (0, 0)) [] [DecAssign WInt (Ident "x" (0, 0)) (Call (Ident "func2" (0, 0)) [] (0, 0)) (0, 0)] (0, 0), Func WInt (Ident "func2" (0,0)) [] [DecAssign WInt (Ident "x" (0,0)) (Call (Ident "func1" (0,0)) [] (0,0)) (0,0)] (0,0)] [])
      `shouldBe` ( ScopeAccum 
                     { scopeMap = fromList [(0,[Ident "func2" (0,0),Ident "func1" (0,0)]),(2,[Ident "x-2" (0,0)]),(4,[Ident "x-4" (0,0)])], 
                       scopeStack = [0], 
                       scopeCounter = 4, 
                       errors = []
                     },
                   Program [Func WInt (Ident "func1" (0,0)) [] [DecAssign WInt (Ident "x-2" (0,0)) (Call (Ident "func2" (0,0)) [] (0,0)) (0,0)] (0,0),Func WInt (Ident "func2" (0,0)) [] [DecAssign WInt (Ident "x-4" (0,0)) (Call (Ident "func1" (0,0)) [] (0,0)) (0,0)] (0,0)] [])

  it "renames parameters" $
    renameParam initialScopeAccum (WInt, Ident "x" (0, 0))
      `shouldBe` (xScopeAccum, (WInt, Ident "x-0" (0, 0)))

  it "renames skip statement" $
    renameStat initialScopeAccum Skip
      `shouldBe` (initialScopeAccum, Skip)

  it "renames declaration assignments" $
    renameStat initialScopeAccum (DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` (xScopeAccum, DecAssign WInt (Ident "x-0" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0))

  it "can't rename declaration assignments for variables already defined in same scope" $
    renameStat xScopeAccum (DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "x-0" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 0,
                       errors = [VariableAlreadyDefined (Ident "x" (0, 0))]
                     },
                   DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)
                 )

  it "renames declaration assignments for variables defined in other scope" $
    renameStat (xScopeAccum {scopeStack = [1, 0]}) (DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "x-0" (0, 0)]), (1, [Ident "x-1" (0, 0)])],
                       scopeStack = [1, 0],
                       scopeCounter = 0,
                       errors = []
                     },
                   DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)
                 )

  it "renames non-declaration assignments" $
    renameStat xScopeAccum (Assign (LIdent (Ident "x" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` (xScopeAccum, Assign (LIdent (Ident "x-0" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))

  it "renames non-declaration assignments in child scopes" $
    renameStat (xScopeAccum {scopeStack = [1, 0]}) (Assign (LIdent (Ident "x" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` (xScopeAccum {scopeStack = [1, 0]}, Assign (LIdent (Ident "x-0" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))

  it "can't rename non-declaration assignments if variable is not defined in scope" $
    renameStat initialScopeAccum (Assign (LIdent (Ident "x" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))
      `shouldBe` (initialScopeAccum {errors = [VariableNotDefined (Ident "x" (0, 0))]}, Assign (LIdent (Ident "x" (0, 0))) (RExpr (IntLiter 1 (0, 0))) (0, 0))

  it "renames read statements" $
    renameStat xScopeAccum (Read (LIdent (Ident "x" (0, 0))) (0, 0))
      `shouldBe` (xScopeAccum, Read (LIdent (Ident "x-0" (0, 0))) (0, 0))

  it "renames free statements" $
    renameStat xScopeAccum (Free (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` (xScopeAccum, Free (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames return statements" $
    renameStat xScopeAccum (Return (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` (xScopeAccum, Return (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames exit statements" $
    renameStat xScopeAccum (Exit (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` (xScopeAccum, Exit (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames print statements" $
    renameStat xScopeAccum (Print (IdentExpr (Ident "x" (0, 0)) (0, 0)))
      `shouldBe` (xScopeAccum, Print (IdentExpr (Ident "x-0" (0, 0)) (0, 0)))

  it "renames println statements" $
    renameStat xScopeAccum (Println (IdentExpr (Ident "x" (0, 0)) (0, 0)))
      `shouldBe` (xScopeAccum, Println (IdentExpr (Ident "x-0" (0, 0)) (0, 0)))

  it "renames if statements" $
    renameStat xScopeAccum (If (IdentExpr (Ident "x" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "x-0" (0, 0)]), (1, [Ident "x-1" (0, 0)]), (2, [Ident "x-2" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 2,
                       errors = []
                     },
                   If (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] [DecAssign WInt (Ident "x-2" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (0, 0)
                 )

  it "renames while statements" $
    renameStat xScopeAccum (While (IdentExpr (Ident "x" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "x-0" (0, 0)]), (1, [Ident "x-1" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 1,
                       errors = []
                     },
                   While (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) [DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)] (0, 0)
                 )

  it "renames begin statements" $
    renameStat initialScopeAccum (Begin [DecAssign WInt (Ident "x" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)])
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(1, [Ident "x-1" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 1,
                       errors = []
                     },
                   Begin [DecAssign WInt (Ident "x-1" (0, 0)) (RExpr (IntLiter 1 (0, 0))) (0, 0)]
                 )

  it "renames lidents" $
    renameLVal xScopeAccum (LIdent (Ident "x" (0, 0)))
      `shouldBe` (xScopeAccum, (LIdent (Ident "x-0" (0, 0))))

  it "renames larrays" $
    renameLVal xScopeAccum (LArray (ArrayElem (Ident "x" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0)))
      `shouldBe` (xScopeAccum, LArray (ArrayElem (Ident "x-0" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0)))

  it "renames lpairs" $
    renameLVal xScopeAccum (LPair (Fst (LIdent (Ident "x" (0, 0))) (0, 0)))
      `shouldBe` (xScopeAccum, LPair (Fst (LIdent (Ident "x-0" (0, 0))) (0, 0)))

  it "renames rexprs" $
    renameRVal xScopeAccum (RExpr (IdentExpr (Ident "x" (0, 0)) (0, 0)))
      `shouldBe` (xScopeAccum, RExpr (IdentExpr (Ident "x-0" (0, 0)) (0, 0)))

  it "renames arrayliters" $
    renameRVal xScopeAccum (ArrayLiter [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0))
      `shouldBe` (xScopeAccum, ArrayLiter [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0))

  it "renames newpairs" $
    renameRVal xScopeAccum (NewPair (IdentExpr (Ident "x" (0, 0)) (0, 0)) (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` (xScopeAccum, NewPair (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames rpairs" $
    renameRVal xScopeAccum (RPair (Fst (LIdent (Ident "x" (0, 0))) (0, 0)))
      `shouldBe` (xScopeAccum, RPair (Fst (LIdent (Ident "x-0" (0, 0))) (0, 0)))

  it "renames calls" $
    renameRVal (initialScopeAccum {scopeMap = fromList [(0, [(Ident "func" (0, 0)), (Ident "x-0" (0, 0))])]}) (Call (Ident "func" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "func" (0, 0), Ident "x-0" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 0,
                       errors = []
                     },
                   Call (Ident "func" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0)
                 )

  it "can't rename function calls if function doesn't exist" $
    renameRVal xScopeAccum (Call (Ident "func" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "x-0" (0, 0)])],
                       scopeStack = [0],
                       scopeCounter = 0,
                       errors = [FunctionNotDefined (Ident "func" (0, 0))]
                     },
                   Call (Ident "func" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0)
                 )

  it "renames identexprs" $
    renameExpr xScopeAccum (IdentExpr (Ident "x" (0, 0)) (0, 0))
      `shouldBe` (xScopeAccum, IdentExpr (Ident "x-0" (0, 0)) (0, 0))

  it "renames arrayexprs" $
    renameExpr xScopeAccum (ArrayExpr (ArrayElem (Ident "x" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0)) (0, 0))
      `shouldBe` (xScopeAccum, ArrayExpr (ArrayElem (Ident "x-0" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0)) (0, 0))

  it "renames unary operator expressions" $
    renameExpr xScopeAccum (Len (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0))
      `shouldBe` (xScopeAccum, Len (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames binary operator expressions" $
    renameExpr xScopeAccum ((:*:) (IdentExpr (Ident "x" (0, 0)) (0, 0)) (IdentExpr (Ident "x" (0, 0)) (0, 0)) (0, 0) )
      `shouldBe` (xScopeAccum, (:*:) (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (IdentExpr (Ident "x-0" (0, 0)) (0, 0)) (0, 0))

  it "renames array elems" $
    renameArrayElem xScopeAccum (ArrayElem (Ident "x" (0, 0)) [IdentExpr (Ident "x" (0, 0)) (0, 0)] (0, 0))
      `shouldBe` (xScopeAccum, ArrayElem (Ident "x-0" (0, 0)) [IdentExpr (Ident "x-0" (0, 0)) (0, 0)] (0, 0))

  it "renames fst pair elems" $
    renamePairElem xScopeAccum (Fst (LIdent (Ident "x" (0, 0))) (0, 0))
      `shouldBe` (xScopeAccum, Fst (LIdent (Ident "x-0" (0, 0))) (0, 0))

  it "renames snd pair elems" $
    renamePairElem xScopeAccum (Snd (LIdent (Ident "x" (0, 0))) (0, 0))
      `shouldBe` (xScopeAccum, Snd (LIdent (Ident "x-0" (0, 0))) (0, 0))

  it "renames and updates scope map for undeclared idents" $
    renameUndeclaredIdent initialScopeAccum (Ident "x" (0, 0))
      `shouldBe` (initialScopeAccum {scopeMap = fromList [(0, [Ident "x-0" (0, 0)])]}, Ident "x-0" (0, 0))

  it "can't rename undeclared ident if it has already been declared in the current scope" $
    renameUndeclaredIdent xScopeAccum (Ident "x" (0, 0))
      `shouldBe` (xScopeAccum {errors = [VariableAlreadyDefined (Ident "x" (0, 0))]}, Ident "x" (0, 0))

  it "renames declared idents" $
    renameDeclaredIdent xScopeAccum (Ident "x" (0, 0))
      `shouldBe` (xScopeAccum, Ident "x-0" (0, 0))

  it "renames declared idents in a child scope" $
    renameDeclaredIdent (xScopeAccum {scopeStack = [2, 1, 0]}) (Ident "x" (0, 0))
      `shouldBe` (xScopeAccum {scopeStack = [2, 1, 0]}, Ident "x-0" (0, 0))

  it "can't rename declared ident if it hasn't been defined in current scope" $
    renameDeclaredIdent initialScopeAccum (Ident "x" (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [],
                       scopeStack = [0],
                       scopeCounter = 0,
                       errors = [VariableNotDefined (Ident "x" (0, 0))]
                     },
                   Ident "x" (0, 0)
                 )

  it "can't rename declared ident if it hasn't been declared in a parent scope" $
    renameDeclaredIdent (xScopeAccum {scopeStack = [3, 2, 1]}) (Ident "x" (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "x-0" (0, 0)])],
                       scopeStack = [3, 2, 1],
                       scopeCounter = 0,
                       errors = [VariableNotDefined (Ident "x" (0, 0))]
                     },
                   Ident "x" (0, 0)
                 )

  it "renames declared ident with closest possible parent scope" $
    renameDeclaredIdent (initialScopeAccum {scopeMap = fromList [(0, [Ident "x-0" (0, 0)]), (1, [Ident "x-1" (0, 0)])], scopeStack = [2, 1, 0]}) (Ident "x" (0, 0))
      `shouldBe` ( ScopeAccum
                     { scopeMap = fromList [(0, [Ident "x-0" (0, 0)]), (1, [Ident "x-1" (0, 0)])],
                       scopeStack = [2, 1, 0],
                       scopeCounter = 0,
                       errors = []
                     },
                   Ident "x-1" (0, 0)
                 )