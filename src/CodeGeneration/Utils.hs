module CodeGeneration.Utils 
  ( (<++>),
    (++>),
    (<++)
  )
where

(<++>) :: Applicative m => m [a] -> m [a] -> m [a]
a <++> b = (++) <$> a <*> b

(++>) :: Applicative m => [a] -> m [a] -> m [a]
a ++> b = (++) a <$> b

(<++) :: Applicative m => m [a] -> [a] -> m [a]
a <++ b = (++) <$> a $ b 