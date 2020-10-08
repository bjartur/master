import Control.Lens.Operators( (&~) )
import Control.Lens.Setter( (.=), mapped, over, set )
import Control.Lens.Tuple( _2 )
import Data.Colour( Colour )
import Data.Colour.Names( black, brown, pink, red, white, yellow )
import Data.Functor( (<&>) )
import Data.Function( (&), on )
import Data.List( transpose, sortBy )
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
import System.FilePath( takeBaseName )

import Bjartur.Types ( PathLines )
import Bjartur.Records

sublists :: [(String,[PathLines])] -> [[(String,[PathLines])]]
sublists universe =
  [ lookups ["technologist", "technician", "Complex3"]
  , lookups ["technologist", "technician", "Simple3", "Medium3", "Complex3"]
  , lookups ["technologist", "technician", "Simple3", "Medium3", "Complex3", "Complex4", "Complex5"]
  , lookups ["technologist", "technician", "Simple3", "Simple4", "Simple5",
            "Medium2", "Medium3", "Medium4", "Medium5",
            "Complex2", "Complex3", "Complex4", "Complex5"] ]
  where
    lookups keys = filter (\(key,_) -> key `elem` keys) universe

hourly:: [(String, [PathLines])] -> [(String, [Double])]
hourly= over (mapped._2.mapped) perhour

perhour :: PathLines -> Double
perhour (polysomnogram, linecount) = fromIntegral linecount / tst (takeBaseName polysomnogram)

plot:: [(Rational, Colour Double)]-> [(String, [Double])]-> Diagram SVG
plot colours distributions=
  renderAxis $ r2Axis &~ do
    axisExtend .= noExtend
    xLabel .= "Participating sleeper"
    xAxis . tickLabelPositions .= []
    xAxis . majorTicks . visible .= False
    xAxis . minorTicks . visible .= False
    yAxis . tickLabelPositions .= zip [0.5..] (distributions <&> fst)
    yAxis . majorTicks . visible .= False
    yAxis . minorTicks . visible .= False
    axisColourMap .= colourScheme colours
    colourBar . visible .= True
    heatMap' (distributions <&> snd)

raw:: [(String, [Double])]-> Diagram SVG
raw= plot [(0,white), (1/3, pink), (2/3, red), (1,brown)]

logscale:: [(String, [Double])]-> Diagram SVG
logscale= over (mapped._2.mapped) (logBase 10) <&> plot [(0,white), (1/3, pink), (2/3, red), (1,brown)]

normalize:: [Double]-> [Double]
normalize list= map (/ sum list) list

normalized:: [(String, [Double])]-> Diagram SVG
normalized= over (mapped._2) normalize <&> plot [(0,yellow), (1,brown)]

render:: ([(String, [Double])]-> Diagram SVG)-> FilePath-> [(String, [Double])]-> IO ()
render draw filename heats=
  renderSVG filename
 (dims zero)
 (rank heats & draw)

sensitivity:: Num score=> (String, [score])-> score
sensitivity = snd <&> sum

severity:: Num score=> [(String, score)]-> score
severity= map snd <&> sum

rank:: [(String, [Double])]-> [(String, [Double])]
rank heats= do
  let -- transposed :: [[(String, (FilePath, score))]] -- score is a scoped type variable
      transposed =
          heats & descending sensitivity
        & map (\(label, list)-> map ((,) label) list)
        & transpose;
  descending severity transposed
        & transpose
        & map (\list-> (list&head&fst, map snd list));

main:: IO ()
main= do
  heats <- numbers <&> tally <&> rank :: IO [(String, [Double])]
  putStrLn "SENSITIVITY"
  putStrLn "The number of minutes between esophageal crescendos, by criteria"
  over (mapped._2) ((60 *) . (tst "total" /) . sum) heats & mapM_ (\(label, meanInterval)-> label ++ "\t" ++ show meanInterval & putStrLn)
  putStrLn "TALLY OF PES CRESCENDOS"
  (heats <&> (\(label, list)-> map ((,) label) list) & transpose) <&> severity & print

  numbers <&> tally >>= render raw "tally.svg"
  numbers <&> tally >>= render logscale "tally.log.svg"
  numbers <&> tally >>= render normalized "tally.distribution.svg"
  numbers <&> hourly >>= render raw "perhour.svg"
  numbers <&> hourly >>= render logscale "perhour.log.svg"
  sequence [martaLines] <&> hourly >>= render raw "marta.svg"
  sequence [martaLines,kaoLines] <&> hourly >>= render raw "manual.svg"
  sequence [martaLines,kaoLines] <&> hourly >>= render normalized "manual.distribution.svg"
  autoscoresLines <&> hourly >>= render raw "autoscores.svg"
  autoscoresLines <&> hourly >>= render normalized "autoscores.distribution.svg"
  lists <- numbers <&> sublists <&> (map hourly)
  sequence_ $ do
    (n, sublist) <- zip [1::Int ..] lists
    [ render raw ("raw" ++ show n ++ ".svg") sublist
     ,render normalized ("distribution" ++ show n ++ ".svg") sublist]

colourScheme:: [(Rational, Colour Double)]-> ColourMap
colourScheme colours= colourMap colours
  & set infColour black
  & set negInfColour (colours & head & snd)

descending:: Ord order=> (a-> order)-> [a]-> [a]
descending accessor= sortBy (flip compare `on` accessor)

tally:: [(String, [PathLines])]
            -> [(String, [Double])]
tally= over (mapped._2.mapped) (fromIntegral.snd)
