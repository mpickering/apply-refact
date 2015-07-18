{-# LANGUAGE BangPatterns #-}
foo = case v of !True -> x
