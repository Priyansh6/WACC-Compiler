module Parsers.Test (test) where

import Text.Megaparsec (parse)

test parser = parse parser ""
