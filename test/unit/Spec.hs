{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main (test) where

import Text.Megaparsec (parse)

test :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
test parser = parse parser ""
