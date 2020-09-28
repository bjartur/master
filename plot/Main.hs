import Control.Applicative( liftA2 )
import Control.Lens.Operators( (&~) )
import Control.Lens.Setter( (.~), (.=), mapped, over, set )
import Control.Lens.Tuple( _1, _2 )
import Control.Monad ( forM )
import Data.Colour( Colour )
import Data.Colour.Names( black, blue, red, white, yellow )
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
import System.Directory( listDirectory )
import System.FilePath( (</>), splitDirectories, takeFileName, takeBaseName )

-- Represent number of lines in a file
type PathLines = (FilePath, Int)

sublists:: IO [[(String,[PathLines])]]
sublists = liftA2 (+++)
  (sequence[manual])
  $ (traverse.traverse) countAllLines $ map (["../csv-to-score/output"]+/+) [
    ["baseline/3"]
  , ["unabrupt", "reversal", "baseline"] +/+ ["3"]
  , (["unabrupt", "reversal"] +/+ ["3"]) ++ (["baseline"] +/+ ["2","3","4","5"])
  , (["unabrupt"] +/+ ["3","4","5"]) ++ (["reversal","baseline"] +/+ ["2","3","4","5"])
  ]

-- a bit like a Cartesian product, but concatenating instead of pairing
(+++):: [[a]]-> [[a]]-> [[a]]
(+++)= liftA2 (++)

numbers:: IO [(String, [PathLines])]
numbers= liftA2 (++) autoscores manual

countLines :: FilePath -> IO PathLines
countLines path = do
  contents <- readFile path
  let lineCount = length . lines $ contents
  pure (path, lineCount)

fromScoreName:: FilePath-> Int
fromScoreName= takeFileName <&> drop (length "VSN-14-080-0") <&> take 2 <&> read

manual:: IO [(String, [PathLines])]
manual= sequence [kao, marta]

autoscores:: IO [(String, [PathLines])]
autoscores= do
  let expandedPaths =
        ["../csv-to-score/output/"]
        +/+ ["unabrupt", "reversal", "baseline"]
        +/+ map pure ['2'..'5']
  forM expandedPaths $ \path -> do
    (a,b) <- countAllLines path
    return (a,b)

kao:: IO (String, [PathLines])
kao= countAllLines "../Nox2score/output/KAÓ/" <&> (_1 .~ "technologist")

marta:: IO (String, [PathLines])
marta= countAllLines "../Nox2score/output/Marta" <&> (_1 .~ "technician")

pairWith:: (a-> b)-> [a]-> [(b,a)]
pairWith f= (map f >>= zip)

rename:: String-> String
rename "unabrupt"= "Simple"
rename "reversal"= "Medium"
rename "baseline"= "Complex"
rename other= other

countAllLines :: FilePath -> IO (String, [(FilePath, Int)])
countAllLines directory= do
  scorePaths <- listDirectory directory
  let enumerated :: [(Int,FilePath)]
      enumerated = pairWith fromScoreName scorePaths
  let forbid = on (liftA2 (&&)) (/=)
  let filtered = filter (fst <&> forbid 13 14) enumerated
  let filenames = filtered <&> snd <&> (directory </>)
  let label = directory & splitDirectories & ((length <&> (subtract 2)) >>= drop) <&> rename & concat
  let values :: IO [PathLines]
      values = traverse countLines filenames
  values <&> (,) label

tally:: [(String, [PathLines])]
            -> [(String, [Double])]
tally= over (mapped._2.mapped) (fromIntegral.snd)

hourly:: [(String, [PathLines])] -> [(String, [Double])]
hourly= over (mapped._2.mapped) perhour

perhour :: PathLines -> Double
perhour (polysomnogram, linecount) = fromIntegral linecount / tst (takeBaseName polysomnogram)

plot:: Colour Double-> [(String, [Double])]-> Diagram SVG
plot colour distributions=
  renderAxis $ r2Axis &~ do
    axisExtend .= noExtend
    xLabel .= "Participating sleeper"
    xAxis . tickLabelPositions .= []
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
  numbers <&> tally >>= render normalized "tally.distribution.svg"
  numbers <&> hourly >>= render raw "perhour.svg"
  sequence [marta] <&> hourly >>= render raw "marta.svg"
  sequence [marta,kao] <&> hourly >>= render raw "manual.svg"
  sequence [marta,kao] <&> hourly >>= render normalized "manual.distribution.svg"
  autoscores <&> hourly >>= render raw "autoscores.svg"
  autoscores <&> hourly >>= render normalized "autoscores.distribution.svg"
  lists <- sublists <&> (map hourly)
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

descending:: Ord order=> (a-> order)-> [a]-> [a]
descending accessor= sortBy (flip compare `on` accessor)

-- Data on the length of polysomnograms
--  TST = Total Sleep Time (for each record)
-- the total total is around 167 hours.
tst :: String -- polysomnogram name
    -> Double -- length in hours
tst "VSN-14-080-001" = 7 + 23/60
tst "VSN-14-080-005" = 6 + 53/60
tst "VSN-14-080-006" = 4 + 55/60
tst "VSN-14-080-007" = 6 + 38/60
tst "VSN-14-080-008" = 5 +  3/60
tst "VSN-14-080-009" = 5 + 41/60
tst "VSN-14-080-010" = 6 + 33/60
tst "VSN-14-080-004" = 6 + 58/60
tst "VSN-14-080-003" = 7 + 8/60
tst "VSN-14-080-011" = 1 + 44/60
tst "VSN-14-080-012" = 6 + 16/60
tst "VSN-14-080-015" = 4 + 23/60
tst "VSN-14-080-016" = 7 + 24/60
tst "VSN-14-080-017" = 7 + 40/60
tst "VSN-14-080-018" = 5 + 32/60
tst "VSN-14-080-019" = 7 +  5/60
tst "VSN-14-080-020" = 6 + 49/60
tst "VSN-14-080-021" = 7 + 36/60
tst "VSN-14-080-022" = 7 + 19/60
tst "VSN-14-080-023" = 7 + 15/60
tst "VSN-14-080-024" = 6 + 58/60
tst "VSN-14-080-025" = 6 + 56/60
tst "VSN-14-080-026" = 6 + 59/60
tst "VSN-14-080-027" = 5 + 41/60
tst "VSN-14-080-028" = 7 + 26/60
tst "VSN-14-080-029" = 6 + 27/60
tst "total" = 166.7
tst f = error $ "Weight for recording name " ++ f ++ " is not defined"

