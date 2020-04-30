{-# LANGUAGE Safe #-}

module Lib (baseline, increasing, decreasing, spans, rises, declines, risesLongerThan, declinesLongerThan, risesLongerThanThree, declinesLongerThanThree, average, range, abrupt, abrupts, baselines, indexBefore, indexOfEndOf, indexAfter, Count, Index, (>$), (>>$)) where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List.Safe ((!!))
import Prelude hiding ((!!))

type Index = Int -- nonnegative
type Count = Int -- positive
--type Debt  = Int -- impositive

-- inclusive
range :: Index-> Index-> [a]-> [a]
range beginning end = drop beginning
                  >>> take count
      where
                   count :: Count
                   count = end - beginning + 1

average :: [Double]-> Double
average list = sum list / (fromIntegral.length) list

-- The output list is one element shorter than the input list.
-- For each overlapping pair of adjacent numbers in the input list,
-- the corresponding Bool in the output list is True iff
-- the former number is strictly less than the latter.
increasing :: [Double]-> [Bool]
increasing [] = []
increasing list = zipWith (<) list (tail list)

-- The output list is one element shorter than the input list.
-- For each overlapping pair of adjacent numbers in the input list,
-- the corresponding Bool in the output list is True iff
-- the former number is strictly greater than the latter.
decreasing :: [Double]-> [Bool]
decreasing [] = []
decreasing list = zipWith (>) list (tail list)

spans :: -- Find and measure each span of True.
        [Bool]->    -- A list which may contain consecutive Trues.
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

rises :: [Double]-> [(Index,Count)]
rises = increasing>>>spans

declines :: [Double]-> [(Index,Count)]
declines = decreasing>>>spans

risesLongerThan :: Int-> [Double]-> [(Index,Count)]
risesLongerThan n list = filter (\(_, count)-> n <= count) (rises list)

declinesLongerThan :: Int-> [Double]-> [(Index,Count)]
declinesLongerThan n list = filter (\(_, count)-> n <= count) (declines list)

risesLongerThanThree :: [Double]-> [(Index,Count)]
risesLongerThanThree list = [ (index,count) | (index, count) <- rises list, count >= 3]

declinesLongerThanThree :: [Double]-> [(Index,Count)]
declinesLongerThanThree list = [ (index,count) | (index, count) <- declines list, count >= 3]

judge :: Int-> [[Double]-> (Index,Count)-> Double-> Bool]-> [Double]-> [(Index,Count)]-> [(Index,Count)]
judge n additional_critera pressures all_candidates = let
    nonEmpty (_, count) _ = 0 < count
    criteria = nonEmpty : (additional_critera <*> [pressures])

    continue :: [(Index,Count)]-> Index-> [(Index,Count)]
    continue [] _ = []
    continue (candidate:candidates) startOfBaseline = do
      let reference = range startOfBaseline (indexBefore candidate) pressures & average
      if and (criteria <*> [candidate] <*> [reference])
      then candidate : continue candidates (indexAfter candidate)
      else do
        let (index, count) = candidate
        if n < count
        then continue ((index+1, count-1):candidates) startOfBaseline
        else continue candidates startOfBaseline
 in
    continue all_candidates 0

baseline :: Int-> [Double]-> [(Index,Count)]-> [(Index,Count)]
baseline n = judge n [belowBaseline, abrupt]

belowBaseline :: [Double]-> (Index,Count)-> Double-> Bool
belowBaseline pressures (index,count) reference = all (reference >) (pressures & drop (index+1) & take count)

abrupt :: [Double]-> (Index,Count)-> Double-> Bool
abrupt pressures decrescendo reference = do
       let pressure = pressures !! indexAfter decrescendo
       maybe False (reference <) pressure

abrupts :: Int-> [Double]-> [(Index,Count)]-> [(Index,Count)]
abrupts n = judge n [abrupt]

baselines :: [Double]-> [(Index,Count)]-> [Double]
baselines pressures candidates = let
            beforeIndices = candidates >$ indexBefore
            afterIndices = candidates >$ indexAfter
            selectors = zipWith range (0:afterIndices) beforeIndices :: [ [Double]-> [Double] ]
    in
            selectors
         >$ ($ pressures)
         >$ average

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
