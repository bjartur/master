{-# LANGUAGE Safe #-}

module Lib (belowBaseline, dipWayBelowBaseline, increasing, decreasing, judge, spans, rises, declines, risesLongerThan, declinesLongerThan, risesLongerThanThree, declinesLongerThanThree, mean, range, abrupt, reversal, indexBefore, indexOfEndOf, indexAfter, {-variance,-} Count, Criteria, Index, (>$), (>>$)) where

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Data.Function ((&), on)
import Data.List.Safe ((!!))
import Data.Maybe (fromJust)
import Prelude hiding ((!!))

type Index = Int -- nonnegative
type Count = Int -- positive
--type Debt  = Int -- impositive

type Criteria = [[Double]-> (Index,Count)-> [Double]-> Bool]

-- inclusive
range :: Index-> Index-> [a]-> [a]
range beginning end = drop beginning
                  >>> take count
      where
                   count :: Count
                   count = end - beginning + 1

mean :: [Double]-> Double
mean list = sum list / (fromIntegral.length) list

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

nonZero  :: [Double]-> (Index,Count)-> [Double]-> Bool
nonZero pressures decrescendo _ =
    map (pressures !!) [indexOfStartOf decrescendo..indexOfEndOf decrescendo]
 & map fromJust & not.elem 0

judge :: Criteria-> Int-> [Double]-> [(Index,Count)]-> [(Index,Count)]
judge additional_critera n pressures all_candidates = let
    nonEmpty (_, count) _ = 0 < count
    criteria = nonEmpty : (nonZero : additional_critera <*> [pressures])

    continue :: [(Index,Count)]-> Index-> [(Index,Count)]
    continue [] _ = []
    continue (candidate:candidates) startOfBaseline = do
      let baseline = range startOfBaseline (indexBefore candidate) pressures & filter (/= 0)
      if and (criteria <*> [candidate] <*> [baseline])
      then candidate : continue candidates (indexAfter candidate)
      else do
        let (index, count) = candidate
        if n < count
        then continue ((index+1, count-1):candidates) startOfBaseline
        else continue candidates startOfBaseline
 in
    continue all_candidates 0

--variance :: [Double] -> Double
--variance numbers = numbers >$ (+ (- mean numbers)) >$ (\difference-> difference*difference) & mean

dipWayBelowBaseline :: [Double]-> (Index,Count)-> [Double]-> Bool
dipWayBelowBaseline pressures candidate baseline= do
       let peak = fromJust (pressures !! indexOfEndOf candidate)
       peak < mean baseline
       -- && variance baseline <= 81 * (mean baseline - peak) ^ (2::Int)

belowBaseline :: [Double]-> (Index,Count)-> [Double]-> Bool
belowBaseline pressures (index,count) baseline = all (mean baseline >) (pressures & drop (index+1) & take count)

abrupt :: [Double]-> (Index,Count)-> [Double]-> Bool
abrupt pressures decrescendo baseline = do
       let pressure = pressures !! indexAfter decrescendo
       maybe False (mean baseline <) pressure

reversal :: [Double]-> (Index,Count)-> [Double]-> Bool
reversal pressures decrescendo _ = do
       let after = indexAfter decrescendo
       let pressure = on (liftA2 max) (pressures !!) after (after+1)
       maybe False ((pressures !! (after - 3) & fromJust) <) pressure

indexBefore :: (Index,Count)-> Index
indexBefore =      fst

indexOfStartOf :: (Index,Count)-> Index
indexOfStartOf (index,_)=      index + 1

indexOfEndOf :: (Index,Count)-> Index
indexOfEndOf =     uncurry (+)

-- `indexAfter span` returns an "index" one beyond the end of span passed in.
-- This index is always beyond the end span,
-- and sometimes beyond the end of the underlying list.
indexAfter :: (Index,Count)-> Index
indexAfter (index,count)=       index + count + 1

(>$) :: Functor l=> l a-> (a-> b)-> l b
(>$) = flip fmap
infixl 1 >$

(>>$) :: (Functor l, Functor m)=> l (m a)-> (a-> b)-> l (m b)
boxed >>$ function =
                   boxed
                >$ fmap function
infixl 2 >>$
