{-# LANGUAGE Safe #-}

module Lib (increasing, decreasing, spans, rises, declines, indicesOfRisesLongerThanThree, indicesOfDeclinesLongerThanThree, average, range, abrupt, indexBefore, indexOfEndOf, indexAfter, Count, Index, (>$)) where

-- Returns all indices of the specified value in a given list.
import Data.List (elemIndices)
import Control.Arrow ((>>>))

type Index = Int -- nonnegative
type Count = Int -- positive
type Debt  = Int -- impositive

-- inclusive
range :: Index -> Index -> [a] -> [a]
range beginning end = drop beginning
                  >>> take count
      where
                   count :: Count
                   count = end - beginning + 1

average :: [Double] -> Double
average list = sum list / (fromIntegral.length) list

-- The output list is one element shorter than the input list.
-- For each overlapping pair of adjacent numbers in the input list,
-- the corresponding Bool in the output list is True iff
-- the former number is strictly less than the latter.
increasing :: [Double] -> [Bool]
increasing [] = []
increasing list = zipWith (<) list (tail list)

-- The output list is one element shorter than the input list.
-- For each overlapping pair of adjacent numbers in the input list,
-- the corresponding Bool in the output list is True iff
-- the former number is strictly greater than the latter.
decreasing :: [Double] -> [Bool]
decreasing [] = []
decreasing list = zipWith (>) list (tail list)

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
                [Int]  -> -- For each prevously found True not preceded by a True, the number of consecutive Trues.
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

declines :: [Double] -> [(Int,Int)]
declines = spans.decreasing

indicesOfRisesLongerThanThree :: [Double] -> [(Int,Int)]
indicesOfRisesLongerThanThree list = [ (index,count) | (index, count) <- rises list, count >= 3]

indicesOfDeclinesLongerThanThree :: [Double] -> [(Int,Int)]
indicesOfDeclinesLongerThanThree list = [ (index,count) | (index, count) <- declines list, count >= 3]

abrupt :: [Double]-> [Double]-> (Int,(Index,Count))-> [(Index,Count)]
abrupt pressures references (number,nadir) =
        if (pressures !! indexAfter nadir) < (references !! number)
        then []
        else [nadir]

indexBefore :: (Index,Count)-> Index
indexBefore =      fst

indexOfEndOf :: (Index,Count)-> Index
indexOfEndOf =     uncurry (+)

indexAfter :: (Index,Count)-> Index
indexAfter =       indexOfEndOf
                >$ (+ 1)

(>$) :: Functor l=> l a-> (a-> b)-> l b
(>$) = flip fmap
infixl 1 >$