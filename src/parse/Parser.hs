module Parser
  ( Parser,
  )
where

import Data.Void
import Text.Megaparsec
import qualified Data.Text as T

type Parser = Parsec Void T.Text