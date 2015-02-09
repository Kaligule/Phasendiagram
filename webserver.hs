-- compile to webserver.cgi

import Network.CGI
import Data.Maybe (fromMaybe)


cgiMain :: String -> CGI CGIResult
cgiMain bildstring = do
        -- read input from web formular
	maybex1 <- readInput "x1" :: CGI (Maybe Double)
	maybex2 <- readInput "x2" :: CGI (Maybe Double)
	maybey1 <- readInput "y1" :: CGI (Maybe Double)
	maybey2 <- readInput "y2" :: CGI (Maybe Double)
        maybeFunctionString <- readInput "function" :: CGI (Maybe String)

        -- replace input with defaults if necessary
        let (x1, x2, y1, y2, functionString) = fillInDefaults (maybex1, maybex2, maybey1, maybey2, maybeFunctionString)

        -- parse function String
        let vectorfield = parseFunktionString functionString

        -- validate input
        let (valid, errormessage) = validateInput x1 x2 y1 y2 bildstring
        -- weisnichtwasdasist <- (setHeader "content-type" "image/svg+xml") 

        if valid
        then output bildstring 
        -- schoener machen
        else outputInternalServerError ["Irgendwas hat nicht funktioniert.", fromMaybe "" errormessage]


main :: IO ()
main = do
        bildstring <- readFile "Phasendiagram.svg"
        runCGI . handleErrors . cgiMain $ bildstring

fillInDefaults :: (Maybe Double, Maybe Double, Maybe Double, Maybe Double,  Maybe String) -> (Double, Double, Double, Double, String)
fillInDefaults (maybex1, maybex2, maybey1, maybey2, maybeFunctionString) = (x1, x2, y1, y2, functionString)
    where
        x1 = fromMaybe (-3) maybex1
        x2 = fromMaybe (-3) maybex1
        y1 = fromMaybe (-3) maybex1
        y2 = fromMaybe (-3) maybex1
        functionString = fromMaybe ("(y,-x)") maybeFunctionString
       
        

-- Man koennte sich noch genauere Tests vorstellen
validateInput :: Double -> Double -> Double -> Double -> String -> (Bool, Maybe String)
validateInput x1 x2 y1 y2 functionString
    | x1 >= x2 = (False, Just "Die untere x-Grenze sollte kleiner sein als die obere.")
    | y1 >= y2 = (False, Just "Die untere y-Grenze sollte kleiner sein als die obere.")
    | (not . elem ',') functionString = (False, Just "Als Funktion bitte eine Funktion mit Signatur (Double, Double) -> (Double, Double) eintragen.")
    | otherwise = (True, Nothing)


 

-- Muss noch implementiert werden, am besten in einem Modul (Funktionsparser)
parseFunktionString :: String -> (Double, Double) -> (Double, Double)
parseFunktionString _ = (\(x,y) -> (x,y))
