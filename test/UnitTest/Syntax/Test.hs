module UnitTest.Syntax.Test (test) where

import Text.Megaparsec (Parsec, parse)
import qualified Text.Megaparsec.Error 

test ::
  Text.Megaparsec.Parsec e s a ->
  s ->
  Either (Text.Megaparsec.Error.ParseErrorBundle s e) a
test parser = parse parser ""
