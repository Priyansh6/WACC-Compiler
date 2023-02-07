module Positions 
  ( Position
  , toPosition ) 
where

import Text.Megaparsec.Pos

type Position = (Int, Int)

toPosition :: SourcePos -> Position
toPosition SourcePos {sourceLine=line, sourceColumn=col}
  = (unPos line, unPos col)