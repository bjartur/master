import Control.Applicative( liftA2 )
import Control.Lens.Operators( (&~) )
import Control.Lens.Setter( (.~), (.=), mapped, over, set )
import Control.Lens.Tuple( _1, _2 )
import Control.Monad ( forM )
import Data.Colour( Colour )
import Data.Colour.Names( black, blue, red, white, yellow )
import Data.Functor( (<&>) )
import Data.Function( (&), on )
import Data.List( transpose, sortBy, sortOn )
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
data PathLines = PathLines { _path :: FilePath, _lines :: Int }
  deriving Show

sublists:: IO [[(String,[PathLines])]]
sublists = (traverse.traverse) tally (map (["../csv-to-score/output"]+/+) [
    ["baseline/3"]
  , ["unabrupt", "reversal", "baseline"] +/+ ["3"]
  , (["unabrupt", "reversal"] +/+ ["3"]) ++ (["baseline"] +/+ map pure ['2'..'5'])
  ])

numbers:: IO [(String, [PathLines])]
numbers= liftA2 (++) autoscores (sequence [kao, marta])

countLines :: FilePath -> IO PathLines
countLines path = do
  contents <- readFile path
  let lineCount = fromIntegral . length . lines $ contents
  pure $ PathLines path lineCount

fromScoreName:: FilePath-> Int
fromScoreName= takeFileName <&> drop (length "VSN-14-080-0") <&> take 2 <&> read

autoscores:: IO [(String, [PathLines])]
autoscores= do
  let expandedPaths =
        ["../csv-to-score/output/"]
        +/+ ["unabrupt", "reversal", "baseline"]
        +/+ map pure ['2'..'5']
  forM expandedPaths $ \path -> do
    (a,b) <- tally path
    return (a,b)

kao:: IO (String, [PathLines])
kao= tally "../Nox2score/output/KAÓ/" <&> (_1 .~ "technologist")

marta:: IO (String, [PathLines])
marta= tally "../Nox2score/output/Marta" <&> (_1 .~ "technician")

pairWith:: (a-> b)-> [a]-> [(b,a)]
pairWith f= (map f >>= zip)

rename:: String-> String
rename "unabrupt"= "Simple"
rename "reversal"= "Medium"
rename "baseline"= "Complex"
rename other= other

tally :: FilePath -> IO (String, [PathLines])
tally directory= do
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

-- Extracts the _lines portion of this structure
discardPath :: [(String, [PathLines])]
            -> [(String, [Double])]
discardPath = over (mapped._2.mapped) (fromIntegral._lines)

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

plot':: Colour Double-> [(String, [PathLines])]-> Diagram SVG
plot' c= plot c . discardPath

raw:: [(String, [PathLines])]-> Diagram SVG
raw= plot' white

normalize:: [Double]-> [Double]
normalize list= map (/ sum list) list

normalized:: [(String, [Double])]-> Diagram SVG
normalized= over (mapped._2) normalize <&> plot yellow

normalized':: [(String, [PathLines])]-> Diagram SVG
normalized' = normalized . discardPath

-- Divides number of events by `tst` of recording
perhour:: [(String, [PathLines])]-> Diagram SVG
perhour = plot white . map (\(n,pc) -> (n, map divideTst pc))
  where
    divideTst :: PathLines -> Double
    divideTst (PathLines name c) = fromIntegral c / tst (takeBaseName name)

render:: ([(String, [PathLines])]-> Diagram SVG)-> FilePath-> [(String, [PathLines])]-> IO ()
render draw filename heats=
  renderSVG filename
 (dims zero)
 (rank heats & draw)

sensitivity:: (String, [PathLines])-> Int
sensitivity = snd <&> map _lines <&> sum

severity:: [(String, PathLines)]-> Int
severity= map (snd <&> _lines) <&> sum

rank:: [(String, [PathLines])]-> [(String, [PathLines])]
rank heats= do
  let transposed :: [[(String, PathLines)]]
      transposed =
          heats & ascending sensitivity
        & map (\(label, list)-> map ((,) label) list)
        & transpose;
  ascending severity transposed
        & transpose
        & map (\list-> (list&head&fst, map snd list));

main:: IO ()
main= do
  heats <- numbers <&> rank :: IO [(String, [PathLines])]
  putStrLn "SENSITIVITY"
  heats <&> sensitivity & print
  putStrLn "TALLY OF PES CRESCENDOS"
  (heats <&> (\(label, list)-> map ((,) label) list) & transpose) <&> severity & print
  numbers >>= render raw "tally.svg"
  numbers >>= render normalized' "tally.distribution.svg"
  numbers >>= render perhour "tally.perhour.svg"
  sequence [marta] >>= render raw "marta.svg"
  sequence [marta,kao] >>= render raw "manual.svg"
  sequence [marta,kao] >>= render normalized' "manual.distribution.svg"
  autoscores >>= render raw "autoscores.svg"
  autoscores >>= render normalized' "autoscores.distribution.svg"
  lists <- sublists
  sequence_ $ do
    (n, sublist) <- zip [1::Int ..] lists
    [ render raw ("raw" ++ show n ++ ".svg") sublist
     ,render normalized' ("distribution" ++ show n ++ ".svg") sublist]

colourScheme:: Colour Double-> ColourMap
colourScheme colour= colourMap [(0,colour), (1,red)]
  & set infColour black
  & set negInfColour blue

(+/+):: [FilePath]-> [FilePath]-> [FilePath]
(+/+)= liftA2 (</>)
infixr 6 +/+ -- one tighter than ++

ascending:: Ord order=> (a-> order)-> [a]-> [a]
ascending accessor= sortBy (flip compare `on` accessor)

descending:: Ord order=> (a-> order)-> [a]-> [a]
descending= sortOn

-- Data on the length of recordings
--  TST = Total Sleep Time
tst :: String -- recording name
    -> Double -- length in hours
tst "VSN-14-080-001" = 7 + 3/60
tst "VSN-14-080-005" = 6 + 3/60
tst "VSN-14-080-006" = 4 + 5/60
tst "VSN-14-080-007" = 6 + 8/60
tst "VSN-14-080-008" = 5 + 3/60
tst "VSN-14-080-009" = 5 + 1/60
tst "VSN-14-080-010" = 6 + 3/60
tst "VSN-14-080-004" = 6 + 8/60
tst "VSN-14-080-003" = 7 + 8/60
tst "VSN-14-080-011" = 1 + 4/60
tst "VSN-14-080-012" = 6 + 6/60
tst "VSN-14-080-015" = 4 + 3/60
tst "VSN-14-080-016" = 7 + 4/60
tst "VSN-14-080-017" = 7 + 0/60
tst "VSN-14-080-018" = 5 + 2/60
tst "VSN-14-080-019" = 7 + 5/60
tst "VSN-14-080-020" = 6 + 9/60
tst "VSN-14-080-021" = 7 + 6/60
tst "VSN-14-080-022" = 7 + 9/60
tst "VSN-14-080-023" = 7 + 5/60
tst "VSN-14-080-024" = 6 + 8/60
tst "VSN-14-080-025" = 6 + 6/60
tst "VSN-14-080-026" = 6 + 9/60
tst "VSN-14-080-027" = 5 + 1/60
tst "VSN-14-080-028" = 7 + 6/60
tst "VSN-14-080-029" = 6 + 7/60
tst f = error $ "Weight for recording name " ++ f ++ " is not defined"
