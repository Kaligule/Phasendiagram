-- compile to webserver.cgi

import Network.CGI
import Data.Maybe (fromMaybe, catMaybes)
import TypeModule (Borders)


cgiMain :: String -> CGI CGIResult
cgiMain bildstring = do
        -- read input from web formular
	maybex1 <- readInput "x1" :: CGI (Maybe Double)
	maybex2 <- readInput "x2" :: CGI (Maybe Double)
	maybey1 <- readInput "y1" :: CGI (Maybe Double)
	maybey2 <- readInput "y2" :: CGI (Maybe Double)
        maybeFunctionString <- readInput "function" :: CGI (Maybe String)

        -- replace input with defaults if necessary
        let (borders, functionString) = fillInDefaults (maybex1, maybex2, maybey1, maybey2) maybeFunctionString

        -- parse function String
        let vectorfield = parseFunktionString functionString

        -- validate input
        let errormessages = validateInput borders bildstring
        -- weisnichtwasdasist <- (setHeader "content-type" "image/svg+xml") 

        -- generate bildstring from parameters borders and vectorfield here

        -- deliver result
        if null errormessages
        then output bildstring
        else outputInternalServerError ("Folgende Sachen haben nicht funktioniert:" : errormessages)


main :: IO ()
main = do
        bildstring <- readFile "Phasendiagram.svg"
        runCGI . handleErrors . cgiMain $ bildstring

fillInDefaults :: (Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> Maybe String -> (Borders, String)
fillInDefaults (maybex1, maybex2, maybey1, maybey2) maybeFunctionString = ((x1, x2, y1, y2), functionString)
    where
        x1 = fromMaybe (-3) maybex1
        x2 = fromMaybe ( 3) maybex2
        y1 = fromMaybe (-3) maybey1
        y2 = fromMaybe ( 3) maybey2
        functionString = fromMaybe ("(y,-x)") maybeFunctionString
       
        

-- Man koennte sich noch genauere Tests vorstellen
validateInput :: Borders -> String -> [String]
validateInput (x1, x2, y1, y2) functionString = catMaybes . map checkRequirements $ requirements
    where
        checkRequirements :: (Bool, String) -> Maybe String
        checkRequirements (correct, errormsg) = if correct then Nothing else Just errormsg

        requirements :: [(Bool, String)]
        requirements = 
            [ (x1 < x2, "Die untere x-Grenze sollte kleiner sein als die obere.")
            , (y1 < y2 , "Die untere y-Grenze sollte kleiner sein als die obere.")
            , (elem ',' functionString, "Als Funktion bitte eine Funktion mit Signatur (Double, Double) -> (Double, Double) eintragen.")
            ]

-- Muss noch implementiert werden, am besten in einem Modul (Funktionsparser)
parseFunktionString :: String -> (Double, Double) -> (Double, Double)
parseFunktionString _ = (\(x,y) -> (x,y))
