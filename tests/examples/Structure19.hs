{-# LANGUAGE BangPatterns #-}
foo = case v of !(x : xs) -> x
