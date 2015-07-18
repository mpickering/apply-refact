-- | The 'mapAndUnzipM' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state-transforming monad.
mapAndUnzipM      :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
{-# INLINE mapAndUnzipM #-}
mapAndUnzipM f xs =  sequence (map f xs) >>= return . foo

ren = (run (foo) run)

