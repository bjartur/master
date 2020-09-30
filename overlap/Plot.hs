module Plot (renderOverlaps) where

import Control.Lens.Operators( (&~) )
import Diagrams.Backend.SVG( SVG, renderSVG )
import Diagrams.Core.Types( Diagram )
import Diagrams.Size( dims )
import Linear.Vector( zero )
import Plots.Types.Bar (groupedBars, onBars, labelBars,  multiBars, stackedBars )
import Plots.Axis( r2Axis, xAxis, xLabel, yAxis)
import Plots.Axis.Render( renderAxis )

drawOverlaps :: [(String,[Double],String)] -> Diagram SVG
drawOverlaps nums = renderAxis $ r2Axis &~ do
    -- Transpose nums in to series of left, overlap, right
    let lefts = map (\(_, [l,_,_] ,_) -> l) nums
    let overlaps = map (\(_, [_,o,_], _) -> o) nums
    let rights = map (\(_, [_,_,r] ,_) -> r) nums
    let series = [ ("Lefts", lefts)
                 , ("Overlaps", overlaps)
                 , ("Rights", rights) ]

    multiBars series snd $ do
        labelBars $ map (\(a,_,b) -> a ++ " " ++ b) nums
        stackedBars

renderOverlaps :: String -> [(String,[Double],String)] -> IO ()
renderOverlaps filename = renderSVG filename (dims zero) . drawOverlaps