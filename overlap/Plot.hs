module Plot (renderOverlaps) where

import Control.Lens.Operators( (&~), (.=) )
import Data.Function( (&) )
import Data.Functor.Identity( Identity(Identity) )
import Diagrams.Backend.SVG( SVG, renderSVG )
import Diagrams.Core.Types( Diagram )
import Diagrams.Size( dims )
import Linear.Vector( zero )
import Plots.Types.Bar (onBars, multiBars, stackedBars )
import Plots.Axis( r2Axis )
import Plots.Axis.Grid( hideGridLines )
import Plots.Axis.Labels( axisLabelText, tickLabelPositions )
import Plots.Axis.Ticks( hideTicks )
import Plots.Types ( key )
import Plots.Axis( xAxis, yAxis )
import Plots.Axis.Render( renderAxis )

data TickState = Ticks { spaceLeft :: Double, itemsLeft :: Int, ticksRight :: [(Double, String)] }
  deriving Show

formatPercentage :: Double -> String
formatPercentage number = ((number*100 & round :: Int) & show) ++ "%"

drawOverlaps :: String -> (Double,Double,Double) -> String -> Diagram SVG
drawOverlaps formerMethod (formerPortion, jaccard, latterPortion) latterMethod = renderAxis $ hideTicks $ r2Axis &~ do
    let series = reverse $
            [ (formerMethod, Identity formerPortion)
            , ("both", Identity jaccard)
            , (latterMethod, Identity latterPortion) ]

    multiBars series snd $ do
        stackedBars
        onBars $ \(statisticName,_) -> key statisticName

    hideGridLines

    let width value = max 0.08 value

    xAxis . axisLabelText .= "Out of all segments matching either Pes crescendo, how much sleep time (%) was matched by which Pes crescendo (color)"

    (xAxis . tickLabelPositions & (.=)) . ticksRight $ foldr
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

renderOverlaps :: FilePath -> String -> (Double,Double,Double) -> String -> IO ()
renderOverlaps filename formerMethod observations latterMethod = renderSVG filename (dims zero) $ drawOverlaps formerMethod observations latterMethod
