{-# LANGUAGE Safe #-}

module Lib (increasing, spans, rises, indicesOfRisesLongerThanThree) where

-- Returns all indices of the specified value in a given list.
import Data.List (elemIndices)

-- Maps the first element to True. Maps a later element to True iff it is strictly greater than the previous element.
increasing :: [Double] -> [Bool]
increasing [] = []
increasing list = zipWith (<) list (tail list)

trueIndices :: [Bool] -> [Int]
trueIndices = elemIndices True


spans :: -- Find and measure each span of True.
        [Bool] ->    -- A list which may contain consecutive Trues.
        [
                (Int,-- The index of a True not preceded by a False.
                Int) -- The number of Trues followed by the first True.
        ]

spans []      = []
spans xs = reverse $ go [] [] 0 False xs where
        go :: -- search for spans of True
                [Int]  -> -- Indices of previously found Trues not preceded by a True.
                [Int]  -> -- For each prevously found True not preceded by a True, the number of trailing Trues.
                Int    -> -- Number of elements already removed from the front of the list.
                Bool   -> -- Last element to be removed (or False, if none has been removed).
                [Bool] -> -- The remainder of the list yet to be searched through.
                [( -- in reverse order, a list of pairs of:
                        Int -- the index of a True not preceed by another True,
                       ,Int -- and the number of consecutive Trues immediately following the True with the above index.
                )]
        go prevs lengths index _ [] = zip prevs lengths
        go prevs lengths index False (False:xs) = go prevs lengths (index+1) False xs
        go prevs lengths index False (True:xs) = go (index:prevs) (1:lengths) (index+1) True xs
        go prevs (currentCount:lengths) index True (True:xs) = go prevs ((currentCount+1):lengths) (index+1) True xs
        go prevs lengths index True (False:xs) = go prevs lengths (index+1) False xs

rises :: [Double] -> [(Int,Int)]
rises = spans.increasing

indicesOfRisesLongerThanThree :: [Double] -> [(Int,Int)]
indicesOfRisesLongerThanThree list = [ (index,count) | (index, count) <- rises list, count >= 3]
