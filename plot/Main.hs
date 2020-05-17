import Control.Applicative( liftA2 )
import Data.Colour.Names( black, blue, magenta, red, white, yellow )
import Data.Functor( (<&>) )
import Data.Function( on )
import Data.List( sort )
import Diagrams.Backend.SVG( SVG, renderSVG )
import Diagrams.Core.Types( Diagram )
import Diagrams.Prelude( (&~), (.=) )
import Diagrams.Size( dims )
import Linear.V2( V2( V2 ) )
import Plots.Axis( Axis, r2Axis, xAxis, xLabel, yAxis, yLabel )
import Plots.Axis.ColourBar( colourBar )
import Plots.Axis.Labels( tickLabelPositions )
import Plots.Axis.Render( renderAxis )
import Plots.Axis.Scale( axisExtend, noExtend )
import Plots.Axis.Ticks( majorTicks, minorTicks )
import Plots.Style( ColourMap, axisColourMap, colourMap )
import Plots.Types( visible )
import Plots.Types.HeatMap( heatMap' )
import System.Directory( listDirectory )
import System.FilePath( (</>), takeFileName )

numbers:: IO [[Double]]
numbers= liftA2 (:) kao autoscores

countLines:: FilePath-> IO Double
countLines= readFile <&> fmap lines <&> fmap length <&> fmap fromIntegral

fromScoreName:: FilePath-> Double
fromScoreName= takeFileName <&> drop (length "VSN-14-080-0") <&> take 2 <&> read

autoscores:: IO [[Double]]
autoscores= traverse (tally fromScoreName) $
  ["../csv-to-score/output/"] +/+ reverse ["unabrupt", "reversal", "baseline"] +/+ reverse (map pure ['2'..'5'])

fromKaoName:: FilePath-> Double
fromKaoName= takeWhile (/='.') <&> read

kao:: IO [Double]
kao= tally fromKaoName "../Nox2score/KAO/"

pairWith:: (a-> b)-> [a]-> [(b,a)]
pairWith f= (map f >>= zip)

tally:: (FilePath-> Double)-> FilePath-> IO [Double]
tally enumerate directory= do
  scorePaths <- listDirectory directory
  let enumerated = pairWith enumerate scorePaths
  let forbid = on (liftA2 (&&)) (/=)
  let filtered = filter (fst <&> forbid 13 14) enumerated
  let sorted = sort filtered
  let filenames = sorted <&> snd <&> (directory </>)
  traverse countLines filenames

plot:: ColourMap-> [[Double]]-> Diagram SVG
plot colourScheme dissimilarities=
  renderAxis $ r2Axis &~ do
    axisExtend .= noExtend
    xAxis . tickLabelPositions .= [(0.5, "1")
                                  ,(5.5, "6"{-7-})
                                  ,(10.5, "11"{-12-})
                                  ,(15.5, "16"{-19-})
                                  ,(20.5, "21"{-24-})
                                  ,(25.5, "26"{-29-})]
    xLabel .= "Participating sleeper"
    xAxis . majorTicks . visible .= False
    xAxis . minorTicks . visible .= False
    yAxis . majorTicks . visible .= False
    yAxis . minorTicks . visible .= False
    axisColourMap .= colourScheme
    colourBar . visible .= True
    heatMap' dissimilarities

raw:: IO (Diagram SVG)
raw= numbers <&> plot whites

normalize:: [Double]-> [Double]
normalize list= map (/ sum list) list

normalized:: IO (Diagram SVG)
normalized= numbers <&> map normalize <&> plot yellows

draw:: IO (Diagram SVG)-> FilePath-> IO ()
a `draw` b= a >>= renderSVG b (dims $ V2 1000 1000.0)

main:: IO ()
main= do
  draw raw "tally.heat.svg"
  draw normalized "distribution.heat.svg"

yellows:: ColourMap
yellows= colourMap [(0,yellow), (1,red)]

whites:: ColourMap
whites= colourMap [(0,white), (1,red)]

(+/+):: [FilePath]-> [FilePath]-> [FilePath]
(+/+)= liftA2 (</>)
