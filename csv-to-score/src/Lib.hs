{-# LANGUAGE Safe #-}

module Lib (increasing, decreasing, spans, rises, declines, risesLongerThanThree, declinesLongerThanThree, average, range, abrupt, indexBefore, indexOfEndOf, indexAfter, Count, Index, (>$), (>>$)) where

import Control.Arrow ((>>>))
import Data.List.Safe ((!!))
import Prelude hiding ((!!))

type Index = Int -- nonnegative
type Count = Int -- positive
--type Debt  = Int -- impositive

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

spans :: -- Find and measure each span of True.
        [Bool] ->    -- A list which may contain consecutive Trues.
        [
                (Int,-- The index of a True not preceded by a False.
                Int) -- The number of Trues followed by the first True.
        ]

spans []      = []
spans booleans = reverse $ go [] [] 0 False booleans where
        go :: -- search for spans of True
                [Int]  -> -- Indices of previously found Trues not preceded by a True.
                [Int]  -> -- For each prevously found True not preceded by a True, the number of consecutive Trues.
                Int    -> -- Number of elements already removed from the front of the list.
                Bool   -> -- Last element to be removed (or False, if none has been removed).
                [Bool] -> -- The remainder of the list yet to be searched through.
                [( -- in reverse order, a list of pairs of:
                        Int -- the index of a True not preceed by another True,
                       ,Int -- and the number of consecutive Trues including the True with the above index.
                )]
        go prevs lengths _     _ [] = zip prevs lengths
        go prevs lengths index False (False:bs) = go prevs lengths (index+1) False bs
        go prevs lengths index False (True:bs) = go (index:prevs) (1:lengths) (index+1) True bs
        go prevs (currentCount:lengths) index True (True:bs) = go prevs ((currentCount+1):lengths) (index+1) True bs
        go prevs lengths index True (False:bs) = go prevs lengths (index+1) False bs
        go _     []      _     True (True:_) = error "Airway resistance detection failed (spans invariant violated)."

rises :: [Double] -> [(Int,Int)]
rises = increasing>>>spans

declines :: [Double] -> [(Int,Int)]
declines = decreasing>>>spans

risesLongerThanThree :: [Double] -> [(Int,Int)]
risesLongerThanThree list = [ (index,count) | (index, count) <- rises list, count >= 3]

declinesLongerThanThree :: [Double] -> [(Int,Int)]
declinesLongerThanThree list = [ (index,count) | (index, count) <- declines list, count >= 3]

-- Note: abrupt returns the empty list if it encounters an out-of-bounds index
-- or if the pressure right after the nadir is less than the reference.
-- Otherwise, it returns a single-item list containing the nadir passed in.
abrupt :: [Double]-> [Double]-> (Int,(Index,Count))-> [(Index,Count)]
abrupt pressures references (number,nadir) = do
        pressure <- pressures !! indexAfter nadir
        reference <- references !! number
        if pressure < reference
        then []
        else [nadir]

indexBefore :: (Index,Count)-> Index
indexBefore =      fst

indexOfEndOf :: (Index,Count)-> Index
indexOfEndOf =     uncurry (+)

-- `indexAfter span` returns an "index" one beyond the end of span passed in.
-- This index is always beyond the end span,
-- and sometimes beyond the end of the underlying list.
indexAfter :: (Index,Count)-> Index
indexAfter =       indexOfEndOf
                >$ (+ 1)

(>$) :: Functor l=> l a-> (a-> b)-> l b
(>$) = flip fmap
infixl 1 >$

(>>$) :: (Functor l, Functor m)=> l (m a)-> (a-> b)-> l (m b)
boxed >>$ function =
                   boxed
                >$ fmap function
infixl 2 >>$
