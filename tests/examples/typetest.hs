
foo
  | False = baz
  | True  = bux

foo a b = ab
  where
    (x:xs) = if baz
                then qux
                else if qux
                  then faux
                  else vrai

p1
  | g1 = e1
  | g2 = e2
  | otherwise = e3


foo = qux (\x -> f (b (c x)))

foo = qux (\xs -> e xs)

foo = qux (\x -> x + b)

foo = qux (\x y -> f y x)

foo = qux (\x -> f (b x))

foo = qux (\x -> f x)

foo = qux (\x -> f $ b x)

foo = qux (\x -> (x +))


