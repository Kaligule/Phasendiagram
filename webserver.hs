-- compile to webserver.cgi

import Network.CGI
import Data.Maybe (fromMaybe, catMaybes)
import TypeModule (Borders, R2Endomorphism)
import FunktionenParser (parseR2toR2)
import Data.Either.Combinators (fromLeft', fromRight')
--import Data.Either (isRight)
import Text.Parsec (ParseError (..))


main :: IO ()
main = runCGI . handleErrors $ cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
        -- read input from web formular
	maybex1 <- readInput "x1" :: CGI (Maybe Double)
	maybex2 <- readInput "x2" :: CGI (Maybe Double)
	maybey1 <- readInput "y1" :: CGI (Maybe Double)
	maybey2 <- readInput "y2" :: CGI (Maybe Double)
        maybeFunctionString <- getInput "function" :: CGI (Maybe String)

        -- replace input with defaults if necessary
        let (borders, functionString) = fillInDefaults (maybex1, maybex2, maybey1, maybey2) maybeFunctionString

        -- parse function String
        let functionParseResult = parseR2toR2 functionString
               
        -- validate input
        let errormessages = validateInput borders functionParseResult
        -- weisnichtwasdasist <- (setHeader "content-type" "image/svg+xml") 




        -- deliver result
        if null errormessages
        then do
            -- generate bildstring from parameters borders and vectorfield here
            bildstring <- liftIO $ computePicture borders (fromRight' functionParseResult)
            output bildstring
        else outputInternalServerError ("Folgende Sachen haben nicht funktioniert:" : errormessages)


-- Do really compute the picture, not just claim it
computePicture :: Borders -> ((Double, Double) -> (Double, Double)) -> IO (String)
computePicture _ _ = readFile "Phasendiagram.svg"

fillInDefaults :: (Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> Maybe String -> (Borders, String)
fillInDefaults (maybex1, maybex2, maybey1, maybey2) maybeFunctionString = ((x1, x2, y1, y2), functionString)
    where
        x1 = fromMaybe (-3) maybex1
        x2 = fromMaybe ( 3) maybex2
        y1 = fromMaybe (-3) maybey1
        y2 = fromMaybe ( 3) maybey2
        functionString = fromMaybe ("(y,2*x)") maybeFunctionString
       
        

-- Man koennte sich noch genauere Tests vorstellen
validateInput :: Borders -> Either ParseError R2Endomorphism -> [String]
validateInput (x1, x2, y1, y2) functionParseResult = catMaybes . map checkRequirements $ requirements
    where
        checkRequirements :: (Bool, String) -> Maybe String
        checkRequirements (correct, errormsg) = if correct then Nothing else Just errormsg

        requirements :: [(Bool, String)]
        requirements = 
            [ (x1 < x2, "Die untere x-Grenze sollte kleiner sein als die obere.")
            , (y1 < y2 , "Die untere y-Grenze sollte kleiner sein als die obere.")
            , (isRight functionParseResult, "Die Funktion konnte nicht geparst werden: " ++ show (fromLeft' functionParseResult) )
            ]

-- Muss noch implementiert werden, am besten in einem Modul (Funktionsparser)
parseFunktionString :: String -> R2Endomorphism
parseFunktionString _ = (\(x,y) -> (x,y))



-- This one should really be imported from somehow. I just didn't manage it, yet

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _       = False


