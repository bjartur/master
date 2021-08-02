module TimeSpec ( spec ) where

import Data.Function ( (&) )
import Test.Hspec( Spec, describe )
import Test.Hspec.QuickCheck( prop )
import Test.QuickCheck( (===) )

import Bjartur.Time

spec :: Spec
spec = do
    let a `minus` b= DateTime (year a - year b) (month a - month b) (day a - day b) (hour a - hour b) (minute a - minute b) (second a - second b)
    describe "instance DateTime Ord" $ do
      prop "considers dates farther from year 1 to be greater" $ do
        let a `lessThan` b= a `minus` b
                          & \datetime->
                              dropWhile (\accessor-> accessor datetime == 0) [year, month, day, hour, minute, second]
                            & \accessors->
                                if null accessors
                                then False
                                else head accessors datetime < 0
        \a b-> (a < b) === (a `lessThan` b)
