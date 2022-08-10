module Utils where

import           Debug.Trace

assert :: Bool -> a -> a
assert False _ = error "assertion failed!"
assert True a  = a

eztrace :: Show a => a -> a
eztrace x = trace (show x) x
