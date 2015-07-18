{-# LANGUAGE TemplateHaskell #-}
main = map $ \ d -> ([| $d |], [| $d |])
