-- compile to webserver.cgi

import Network.CGI

cgiMain :: String -> CGI CGIResult
cgiMain bildstring = do
	--s <- queryString
	x1 <- getInput "x1"
	x2 <- getInput "x2"
	y1 <- getInput "y1"
	y2 <- getInput "y2"
	setHeader "content-type" "image/svg+xml"
	output $ bildstring

main :: IO ()
main = do
        bild <- readFile "Phasendiagram.svg"
        runCGI (handleErrors (cgiMain bild))
