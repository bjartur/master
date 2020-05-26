import Control.Applicative( liftA2 )
import Control.Lens.Operators( (&~) )
import Control.Lens.Setter( (.~), (.=), mapped, over, set )
import Control.Lens.Tuple( _1, _2 )
import Data.Colour( Colour )
import Data.Colour.Names( black, blue, red, white, yellow )
import Data.Functor( (<&>) )
import Data.Function( (&), on )
import Data.List( intercalate, sort )
import Diagrams.Backend.SVG( SVG, renderSVG )
import Diagrams.Core.Types( Diagram )
import Diagrams.Size( dims )
import Linear.V2( V2( V2 ) )
import Plots.Axis( r2Axis, xAxis, xLabel, yAxis)
import Plots.Axis.ColourBar( colourBar )
import Plots.Axis.Labels( tickLabelPositions )
import Plots.Axis.Render( renderAxis )
import Plots.Axis.Scale( axisExtend, noExtend )
import Plots.Axis.Ticks( majorTicks, minorTicks )
import Plots.Style( ColourMap, axisColourMap, colourMap, infColour, negInfColour )
import Plots.Types( visible )
import Plots.Types.HeatMap( heatMap' )
import System.Directory( listDirectory )
import System.FilePath( (</>), splitDirectories, takeFileName )

numbers:: IO [(String, [Double])]
numbers= liftA2 (++) autoscores $ sequence [kao, marta]

countLines:: FilePath-> IO Double
countLines= readFile <&> fmap lines <&> fmap length <&> fmap fromIntegral

fromScoreName:: FilePath-> Double
fromScoreName= takeFileName <&> drop (length "VSN-14-080-0") <&> take 2 <&> read

autoscores:: IO [(String, [Double])]
autoscores= traverse (tally fromScoreName) $
  ["../csv-to-score/output/"] +/+ ["unabrupt", "reversal", "baseline"] +/+ (map pure ['2'..'5'])

fromKaoName:: FilePath-> Double
fromKaoName= takeWhile (/='.') <&> read

kao:: IO (String, [Double])
kao= tally fromKaoName "../Nox2score/KAO/" <&> (_1 .~ "KAÓ")

fromMartaName:: FilePath-> Double
fromMartaName= drop (length "VSN-14-080-0") <&> fromKaoName

marta:: IO (String, [Double])
marta= tally fromMartaName "../Nox2score/Marta" <&> (_1 .~ "Marta")

pairWith:: (a-> b)-> [a]-> [(b,a)]
pairWith f= (map f >>= zip)

tally:: (FilePath-> Double)-> FilePath-> IO (String, [Double])
tally enumerate directory= do
  scorePaths <- listDirectory directory
  let enumerated = pairWith enumerate scorePaths
  let forbid = on (liftA2 (&&)) (/=)
  let filtered = filter (fst <&> forbid 13 14) enumerated
  let sorted = sort filtered
  let filenames = sorted <&> snd <&> (directory </>)
  let label = directory & splitDirectories & ((length <&> (subtract 2)) >>= drop) & intercalate "#"
  let values = traverse countLines filenames
  values <&> (,) label


plot:: Colour Double-> [(String, [Double])]-> Diagram SVG
plot colour distributions=
  renderAxis $ r2Axis &~ do
    axisExtend .= noExtend
    xLabel .= "Participating sleeper"
    xAxis . tickLabelPositions .= [(0.5, "#1")
                                  ,(5.5, "#6"{-7-})
                                  ,(10.5, "#11"{-12-})
                                  ,(15.5, "#16"{-19-})
                                  ,(20.5, "#21"{-24-})
                                  ,(25.5, "#26"{-29-})]
    xAxis . majorTicks . visible .= False
    xAxis . minorTicks . visible .= False
    yAxis . tickLabelPositions .= zip [0.5..] (distributions <&> fst)
    yAxis . majorTicks . visible .= False
    yAxis . minorTicks . visible .= False
    axisColourMap .= colourScheme colour
    colourBar . visible .= True
    heatMap' (distributions <&> snd)

raw:: IO (Diagram SVG)
raw= numbers <&> plot white

normalize:: [Double]-> [Double]
normalize list= map (/ sum list) list

normalized:: IO (Diagram SVG)
normalized= numbers <&> over (mapped._2) normalize <&> plot yellow

draw:: IO (Diagram SVG)-> FilePath-> IO ()
a `draw` b= a >>= renderSVG b (dims $ V2 1000 1000.0)

main:: IO ()
main= do
  draw raw "tally.heat.svg"
  draw normalized "distribution.heat.svg"

colourScheme:: Colour Double-> ColourMap
colourScheme colour= colourMap [(0,colour), (1,red)]
  & set infColour black
  & set negInfColour blue

(+/+):: [FilePath]-> [FilePath]-> [FilePath]
(+/+)= liftA2 (</>)
