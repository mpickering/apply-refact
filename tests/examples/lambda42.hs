f :: Bool -> Integer
f = (\ b ->  (case b of
      False -> 1
      True -> 0))