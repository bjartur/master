module Plot (formatPercentage, renderOverlaps, renderBarPlot) where

import Control.Applicative( liftA2 )
import Control.Lens.Operators( (&~), (.=) )
import Data.Function( (&), on )
import Data.Functor( (<&>) )
import Data.Functor.Identity( Identity(Identity) )
import Data.List( sortOn )
import Data.Ratio( Ratio, numerator, denominator )
import Diagrams.Backend.SVG( SVG, renderSVG )
import Diagrams.Core.Types( Diagram )
import Diagrams.Size( dims )
import Linear.Vector( zero )
import Plots.Types.Bar (onBars, namedBarPlot', multiBars, stackedBars )
import Plots.Axis( r2Axis )
import Plots.Axis.Grid( hideGridLines )
import Plots.Axis.Labels( axisLabelText, tickLabelPositions )
import Plots.Axis.Ticks( hideTicks )
import Plots.Types ( key )
import Plots.Axis( xAxis, yAxis )
import Plots.Axis.Render( renderAxis )

import Bjartur.Time( DateTime )
import Histogram( combineHistograms, discreteHistogram )

data TickState = Ticks { spaceLeft :: Ratio Int, itemsLeft :: Int, ticksRight :: [(Ratio Int, String)] }
  deriving Show

formatPercentage :: Ratio Int -> String
formatPercentage number = ((number*100 & round :: Int) & show) ++ "%"

box :: (String, Ratio Int) -> (String, Identity Double)
box = fmap (Identity . toDouble)

toDouble :: Ratio Int -> Double
toDouble = on (liftA2 (/)) (fmap fromIntegral) numerator denominator

drawOverlaps :: String -> (Ratio Int,Ratio Int,Ratio Int) -> String -> Diagram SVG
drawOverlaps formerMethod (formerPortion, jaccard, latterPortion) latterMethod = renderAxis $ hideTicks $ r2Axis &~ do
    let series = reverse $ map box
            [ (formerMethod, formerPortion)
            , ("both", jaccard)
            , (latterMethod, latterPortion) ]

    multiBars series snd $ do
        stackedBars
        onBars $ \(statisticName,_) -> key statisticName

    hideGridLines

    let width value = max 0.08 value

    xAxis . axisLabelText .= "Out of all segments matching either Pes crescendo, how much sleep time (%) was matched by which Pes crescendo (color)"

    (xAxis . tickLabelPositions & (.=)) . map (\(fraction, label) -> (toDouble fraction, label)) . ticksRight $ foldr
      (\next state->
        state {
          spaceLeft = spaceLeft state - width next,
          itemsLeft = itemsLeft state - 1,
          ticksRight = (spaceLeft state - width next/2, formatPercentage next):ticksRight state
        })
      Ticks { spaceLeft = 1, itemsLeft = 3, ticksRight = [] }
      [formerPortion, jaccard, latterPortion]

    yAxis . axisLabelText .= formatPercentage (jaccard / (jaccard + min formerPortion latterPortion)) ++ "*"
    yAxis . tickLabelPositions .= []

svg :: FilePath-> Diagram SVG-> IO ()
svg filename = renderSVG filename (dims zero)

renderOverlaps :: FilePath -> String -> (Ratio Int,Ratio Int,Ratio Int) -> String -> IO ()
renderOverlaps filename formerMethod observations latterMethod = renderSVG filename (dims zero) $ drawOverlaps formerMethod observations latterMethod

drawBarPlot :: [[[(DateTime, DateTime)]]]-> Diagram SVG
drawBarPlot agreements = do
      let arrangeBarsByValue xs = sortOn snd xs
      let bars = agreements <&> discreteHistogram & combineHistograms & arrangeBarsByValue
      do
        hideGridLines
        bars <&> fmap (fromIntegral<&>(/(60*60))) & namedBarPlot'
      &  (&~) r2Axis & renderAxis -- (&~) returns the final state after drawing the namedBarPlot'

renderBarPlot :: FilePath-> [[[(DateTime, DateTime)]]]-> IO ()
renderBarPlot filename = drawBarPlot <&> svg filename
