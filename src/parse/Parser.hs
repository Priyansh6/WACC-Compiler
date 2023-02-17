module Parser
  ( Parser
  , liftPos1
  , liftPos2
  , liftPos3
  , liftPosScopeFunc
  , liftPosScopeIf
  , liftPosScopeWhile
  , liftScopeBegin
  , deferLiftPos1
  , deferLiftPos2
  , deferLiftPos3
  , getPosition
  )
where

import AST (Position, Scope)
import Control.Applicative ((<**>))
import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, getSourcePos)
import Text.Megaparsec.Pos
import qualified Data.Text as T

type Parser = Parsec Void T.Text

liftPos1 :: (a -> Position -> c) -> Parser a -> Parser c
liftPos1 cons p = getPosition <**> (cons <$> p)

liftPos2 :: (a -> b -> Position -> c) -> Parser a -> Parser b -> Parser c
liftPos2 cons p1 p2 = getPosition <**> (cons <$> p1 <*> p2)

liftPos3 :: (a -> b -> c -> Position -> d) -> Parser a -> Parser b -> Parser c -> Parser d
liftPos3 cons p1 p2 p3 = getPosition <**> (cons <$> p1 <*> p2 <*> p3)

liftPosScopeFunc :: (a -> b -> c -> d -> Scope -> Position -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
liftPosScopeFunc cons p1 p2 p3 p4 = getPosition <**> (cons <$> p1 <*> p2 <*> p3 <*> p4 <*> pure Nothing)

liftPosScopeIf :: (a -> b -> Scope -> c -> Scope -> Position -> d) -> Parser a -> Parser b -> Parser c -> Parser d
liftPosScopeIf cons p1 p2 p3 = getPosition <**> (cons <$> p1 <*> p2 <*> pure Nothing <*> p3 <*> pure Nothing)

liftPosScopeWhile :: (a -> b -> Scope -> Position -> c) -> Parser a -> Parser b -> Parser c
liftPosScopeWhile cons p1 p2 = getPosition <**> (cons <$> p1 <*> p2 <*> pure Nothing)

deferLiftPos1 :: (a -> Position -> b) -> Parser (a -> b)
deferLiftPos1 cons = flip cons <$> getPosition

deferLiftPos2 :: (a -> b -> Position -> c) -> Parser (a -> b -> c)
deferLiftPos2 cons = (\c a b -> cons a b c) <$> getPosition

deferLiftPos3 :: (a -> b -> c -> Position -> d) -> Parser (a -> b -> c -> d)
deferLiftPos3 cons = (\d a b c -> cons a b c d) <$> getPosition

liftScopeBegin :: (a -> Scope -> b) -> Parser a -> Parser b
liftScopeBegin cons p1 = cons <$> p1 <*> pure Nothing

toPosition :: SourcePos -> Position
toPosition SourcePos {sourceLine=line, sourceColumn=col}
  = (unPos line, unPos col)

getPosition :: Parser Position
getPosition = getSourcePos <&> toPosition