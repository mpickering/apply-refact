{-# LANGUAGE BangPatterns #-}
foo = case v of !(Just x) -> x
