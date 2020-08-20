import Control.Applicative( liftA2 )
import Control.DeepSeq ( ($!!) )
import Control.Lens.Operators( (&~) )
import Control.Lens.Setter( (.~), (.=), mapped, over, set )
import Control.Lens.Tuple( _1, _2 )
import Control.Monad ( forM )
import Data.Colour( Colour )
import Data.Colour.Names( black, blue, red, white, yellow )
import Data.Functor( (<&>) )
import Data.Function( (&), on )
import Data.List( intercalate, sort )
import Diagrams.Backend.SVG( SVG, renderSVG )
import Diagrams.Core.Types( Diagram )
import Diagrams.Size( dims )
import Linear.Vector( zero )
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

sublists:: IO [[(String,[Double])]]
sublists = (traverse.traverse) tally (map (["../csv-to-score/output"]+/+) [
    ["baseline/3"]
  , ["unabrupt", "reversal", "baseline"] +/+ ["3"]
  , (["unabrupt", "reversal"] +/+ ["3"]) ++ (["baseline"] +/+ map pure ['2'..'5'])
  ])

numbers:: IO [(String, [Double])]
numbers= liftA2 (++) autoscores $ sequence [kao, marta]

countLines:: FilePath-> IO Double
countLines= readFile <&> fmap lines <&> fmap length <&> fmap fromIntegral

fromScoreName:: FilePath-> Double
fromScoreName= takeFileName <&> drop (length "VSN-14-080-0") <&> take 2 <&> read

autoscores:: IO [(String, [Double])]
autoscores= do
  let expandedPaths =
        ["../csv-to-score/output/"]
        +/+ ["unabrupt", "reversal", "baseline"]
        +/+ map pure ['2'..'5']
  forM expandedPaths $ \path -> do
    (a,b) <- tally path
    return $!! (a,b)

kao:: IO (String, [Double])
kao= tally "../Nox2score/output/KAÓ/" <&> (_1 .~ "KAÓ")

marta:: IO (String, [Double])
marta= tally "../Nox2score/output/Marta" <&> (_1 .~ "Marta")

pairWith:: (a-> b)-> [a]-> [(b,a)]
pairWith f= (map f >>= zip)

tally:: FilePath-> IO (String, [Double])
tally directory= do
  scorePaths <- listDirectory directory
  let enumerated = pairWith fromScoreName scorePaths
  let forbid = on (liftA2 (&&)) (/=)
  let filtered = filter (fst <&> forbid 13 14) enumerated
  let sorted = sort filtered
  let filenames = sorted <&> snd <&> (directory </>)
  let label = directory & splitDirectories & ((length <&> (subtract 2)) >>= drop) & intercalate "#"
  let values :: IO [Double]
      values = traverse countLines filenames
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

raw:: [(String, [Double])]-> Diagram SVG
raw= plot white

normalize:: [Double]-> [Double]
normalize list= map (/ sum list) list

normalized:: [(String, [Double])]-> Diagram SVG
normalized= over (mapped._2) normalize <&> plot yellow

render:: ([(String, [Double])]-> Diagram SVG)-> FilePath-> [(String, [Double])]-> IO ()
render draw filename heats= renderSVG filename (dims zero) (draw heats)

main:: IO ()
main= do
  numbers >>= render raw "tally.svg"
  numbers >>= render normalized "tally.distribution.svg"
  sequence [marta] >>= render raw "marta.svg"
  sequence [marta,kao] >>= render raw "manual.svg"
  sequence [marta,kao] >>= render normalized "manual.distribution.svg"
  autoscores >>= render raw "autoscores.svg"
  autoscores >>= render normalized "autoscores.distribution.svg"
  lists <- sublists
  sequence_ $ do
    (n, sublist) <- zip [1::Int ..] lists
    [ render raw ("raw" ++ show n ++ ".svg") sublist
     ,render normalized ("distribution" ++ show n ++ ".svg") sublist]

colourScheme:: Colour Double-> ColourMap
colourScheme colour= colourMap [(0,colour), (1,red)]
  & set infColour black
  & set negInfColour blue

(+/+):: [FilePath]-> [FilePath]-> [FilePath]
(+/+)= liftA2 (</>)
infixr 6 +/+ -- one tighter than ++
