module TypesSpec ( spec ) where

import Data.Function ( (&) )
import Test.Hspec
import Test.QuickCheck ( property )

import Bjartur.Types

spec :: Spec
spec = do
    let a `minus` b= DateTime (year a - year b) (month a - month b) (day a - day b) (hour a - hour b) (minute a - minute b) (second a - second b)
    describe "instance DateTime Ord" $ do
      it "considers dates farther from year 1 to be greater" $ do
        let a `lessThan` b= a `minus` b
                          & \datetime->
                              dropWhile (\accessor-> accessor datetime == 0) [year, month, day, hour, minute, second]
                            & \accessors->
                                if null accessors
                                then False
                                else head accessors datetime < 0
        property $ \a b-> (a < b) == (a `lessThan` b)