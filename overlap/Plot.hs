module Plot (renderOverlaps) where

import Control.Lens.Operators( (&~) )
import Data.Functor.Identity( Identity(Identity) )
import Diagrams.Backend.SVG( SVG, renderSVG )
import Diagrams.Core.Types( Diagram )
import Diagrams.Size( dims )
import Linear.Vector( zero )
import Plots.Types.Bar (onBars, multiBars, stackedBars )
import Plots.Axis( r2Axis )
import Plots.Axis.Grid( hideGridLines )
import Plots.Axis.Ticks( hideTicks )
import Plots.Types ( key )
import Plots.Axis.Render( renderAxis )

drawOverlaps :: String -> (Double,Double,Double) -> String -> Diagram SVG
drawOverlaps formerMethod (formerPortion, jaccard, latterPortion) latterMethod = renderAxis $ hideTicks $ r2Axis &~ do
    let series = reverse $
            [ (formerMethod, Identity formerPortion)
            , ("Jaccard*", Identity jaccard)
            , (latterMethod, Identity latterPortion) ]

    multiBars series snd $ do
        stackedBars
        onBars $ \(statisticName,_) -> key statisticName
    hideGridLines

renderOverlaps :: FilePath -> String -> (Double,Double,Double) -> String -> IO ()
renderOverlaps filename formerMethod observations latterMethod = renderSVG filename (dims zero) $ drawOverlaps formerMethod observations latterMethod
