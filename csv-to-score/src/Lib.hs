{-# LANGUAGE Safe #-}

module Lib where

-- Returns all indices of the specified value in a given list.
import Data.List (elemIndices)

-- Maps the first element to True. Maps a later element to True iff it is strictly greater than the previous element.
increasing :: [Double] -> [Bool]
increasing [] = []
increasing list = True : zipWith (<) list (tail list)

trueIndices :: [Bool] -> [Int]
trueIndices = elemIndices True

diffs :: [Int] -> [(Int,Int)]
diffs list = zip list $ zipWith (-) (tail list) list
