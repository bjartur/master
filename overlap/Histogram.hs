module Histogram where

import Control.Monad( ap )
import Data.Function( (&) )
import Data.Functor( (<&>) )
import Data.List( foldl1' )
import qualified Data.Map as BinTree( fromListWith, insertWith, toList )

import Bjartur( numberOfPatterns )
import Bjartur.Time( DateTime, measure' )

flatten :: (Ord a, Show a)=> [(a, a)]-> [a]
flatten [] = []
flatten ((a,b):xs) = a:b:flatten xs

merge :: (Ord a, Show a)=> [a]-> [a]-> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) = merge' a as b bs (compare a b)

merge' :: (Ord a, Show a)=> a-> [a]-> a-> [a]-> Ordering-> [a]
merge' a as _ bs EQ = a:merge as bs
merge' a as b bs LT = a:merge as (b:bs)
merge' a as b bs GT = b:merge (a:as) bs

mergeAll :: (Ord a, Show a)=> [[a]]-> [a]
mergeAll [] = []
mergeAll [x] = x
mergeAll (as:bs:rest) = mergeAll ((merge as bs):rest)

flattenAll :: (Ord a, Show a)=> [[(a,a)]] -> [a]
flattenAll xs = map flatten xs & mergeAll

labelFromOutside :: (Ord a, Show a)=> [a]-> [(a, a)]-> [Bool]
labelFromOutside [] [] = []
labelFromOutside [_] [] = []
labelFromOutside [] datetime = error $ "DateTime " ++ show datetime ++ " out of bounds!"
labelFromOutside (_:a:llir) [] = False : labelFromOutside (a:llir) []
labelFromOutside (núverandi:allir) ((upphaf,endir):eini) = if núverandi < upphaf then False : labelFromOutside allir ((upphaf,endir):eini) else True : labelFromInside allir ((upphaf,endir):eini)


labelFromInside :: (Ord a, Show a)=> [a]-> [(a, a)]-> [Bool]
labelFromInside [] _ = error "Closing datetime lost!"
labelFromInside [_] [_]= []
labelFromInside _ [] = error "labelFromInside called without the open interval!"
labelFromInside (núverandi:allir) ((_,endir):eini) = if núverandi < endir then True : labelFromInside allir ((undefined,endir):eini) else False : labelFromOutside allir eini

-- inputs: List of lists of closed intervals and the ascending list of all of the endpoints of those intervals, without repetition.
-- output: For each pair of adjacent endpoints, how many of the intervals contain both endpoints?
-- The output is a list one shorter than the list of endpoints. It can be shorter or longer than the list of intervals.
count :: (Ord a, Show a)=> [[(a,a)]]-> [a]-> [Int]
count [] [] = []
count xs flati =  do
  let labelOne = labelFromOutside flati
  let allLabeled = map labelOne xs ::[[Bool]]
  let sane = all (length<&>(==length(allLabeled!!0))) allLabeled
  let value = allLabeled <&> map fromEnum & foldl1' (zipWith (+))
  if sane then value else error "count: Not all input lists of equal length!"

unflatten :: (Ord a, Show a)=> [a]-> [(a,a)]
unflatten = ap zip tail

flatCount :: (Ord a, Show a)=> [[(a,a)]]-> [(Int,(a,a))]
flatCount xs = do
  let flat = flattenAll xs
  zip (count xs flat) (unflatten flat)

apply :: [a-> a]-> a-> a
apply [] v = v
apply (f:fs) v = apply fs (f v)

treesum :: [(String, Int)]-> [(String, Int)]
treesum = BinTree.fromListWith (+)
  <&> (apply $ map (flip (BinTree.insertWith (flip const)) 0 . show) [0..numberOfPatterns])
  <&> BinTree.toList

discreteHistogram :: [[(DateTime, DateTime)]]-> [(String, Int)]
discreteHistogram eventsByPattern = do
         flatCount eventsByPattern
     <&> (\(int, period)-> (show int, measure' period))
      &  treesum

combineHistograms :: [[(String,Int)]]-> [(String,Int)]
combineHistograms =
         concat <&> treesum
