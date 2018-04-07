{-# LANGUAGE Safe #-}

module Lib
    ( increasing
    ) where

-- Maps the first element to True. Maps a later element to True iff it is strictly greater than the previous element.
increasing :: [Double] -> [Bool]
increasing [] = []
increasing list = True : zipWith (<) list (tail list)
