{-# LANGUAGE OverloadedStrings #-}

module CodeGeneration.DataSection (generateDataSection) where

import Control.Monad.State
import CodeGeneration.IR
import CodeGeneration.Utils (LiterTable)
import AST

import qualified Data.Map as M
import qualified Data.Text as T

type DataSegmentGen = State DataAux

data DataAux = DataAux { 
  labelNumber :: Int,
  literTable :: LiterTable,
  dataSection :: [Data],
  funcName :: T.Text
  }

generateDataSection :: AST.Stats -> T.Text -> ([Data], LiterTable)
generateDataSection ss name = (ds, lt)
    where 
        dataAux = execState (generateDataSection' ss) (DataAux {labelNumber = 0, literTable = M.empty, dataSection = [], funcName = name})
        ds = dataSection dataAux
        lt = literTable dataAux

generateDataSection' :: AST.Stats -> DataSegmentGen ()
generateDataSection' ss = mapM_ generateStatData ss 

generateStatData :: AST.Stat -> DataSegmentGen ()
generateStatData (DecAssign _ _ rval _) = generateRValData rval
generateStatData (Assign _ rval _) = generateRValData rval
generateStatData (Return expr _) = generateExprData expr
generateStatData (Print expr) = generateExprData expr
generateStatData (Println expr) = generateExprData expr
generateStatData (If expr ss1 _ ss2 _ _) = generateExprData expr >> generateDataSection' ss1 >> generateDataSection' ss2
generateStatData (While expr ss _ _) = generateExprData expr >> generateDataSection' ss
generateStatData (Begin ss _) = generateDataSection' ss
generateStatData _ = return ()

generateRValData :: AST.RVal -> DataSegmentGen ()
generateRValData (RExpr expr ) = generateExprData expr
generateRValData (NewPair e1 e2 _) = generateExprData e1 >> generateExprData e2
generateRValData (Call _ exprs _) = mapM_ generateExprData exprs
generateRValData _ = return ()

generateExprData :: AST.Expr -> DataSegmentGen ()
generateExprData (StrLiter text _) = do
  updateAuxData text
generateExprData _ = return ()

updateAuxData :: T.Text -> DataSegmentGen ()
updateAuxData text = do
  label <- nextLabel
  insertToLiterTable label text
  insertData (StringData label text)

insertData :: Data -> DataSegmentGen ()
insertData d = do
  modify (\a@DataAux {dataSection = ds} -> (a {dataSection = d : ds}))


nextLabel :: DataSegmentGen Label
nextLabel = nextLabelId >>= toLabel
  where
    nextLabelId :: State DataAux Int
    nextLabelId = state (\a@DataAux {labelNumber = l} -> (l, a {labelNumber = l + 1}))

    toLabel :: Int -> State DataAux Label
    toLabel i = gets (\DataAux {funcName = x} -> ".L._" <> x <> "_str" <> T.pack (show i))

insertToLiterTable :: Label -> T.Text -> DataSegmentGen ()
insertToLiterTable l t = modify (\a@DataAux {literTable = lt} -> a {literTable = M.insert t l lt})


