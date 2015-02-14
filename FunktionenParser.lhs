
Funktionen parsen
=================

Wir wollen einen Term parsen, der Variablen 'x' und 'y' enthaelt.
Dabei probieren wir gleich mal aus, was wir aus diesem Tutorial lernen koennen: http://unbui.lt/#!/post/haskell-parsec-basics

Parsec soll die meiste Arbeit fuer uns machen

> import Text.Parsec as Parsec
> import Text.Parsec ((<?>), spaces, string, char, try)
> import Control.Monad.Identity (Identity)
> import Data.List (intercalate)

> par rule text = Parsec.parse rule "Funktionenstring" text

> main :: IO()
> main = do
>     let stringToParse = "(x*y)-y +x"
>     -- let termParseResult = (parseTerm stringToParse) -- :: Term Double
>     let termParseResult = (par parseTerm stringToParse) :: Either ParseError (Term Double) 
>     case termParseResult of
>         Right term -> putStrLn $ "Success: " ++ show term
>         Left errormessage -> print errormessage

Interessante Strings koennten sein: 

* "(x*y)-y +x"
* "x+x+x+x+y*y"
* "x+x+x+x+y*y+x"

Wir wollen einfache Funktionen mit Num-Argumenten parsen. Dividieren ist erstmal verboten bis ich mir ueberlegt habe was ich mit Nullnennern mache. Dannach waere es ziemlich genau so zu implementieren wie Sub

> data Term a = Con a           -- some constant 
>             | VarX            -- Variable x
>             | VarY            -- Variable y
>             | Add [Term a] -- addieren
>             | Sub (Term a) (Term a) -- subtrahieren
>             | Mul [Term a] -- multiplizieren
> --             | Div (Term a) (Term a) -- dividieren
>             | Pow (Term a) (Term a) -- potenzieren
>             | Sin (Term a)          -- sinus
>             | Cos (Term a)          -- cosinus
>             | Tan (Term a)          -- tangens


Kann man aufschreiben, wird man aber spaeter nicht brauchen.

> instance (Show a) => Show (Term a)
>     where
>         show (Con a) = show a
>         show VarX = "x"
>         show VarY = "y"
>         show (Add sumands) = inBrackets . intercalate " + " . map show $ sumands
>         show (Sub x y) = inBrackets $ show x ++ " - " ++ show y
>         show (Pow x y) = inBrackets $ show x ++ " ^ " ++ show y
>         show (Mul factors) = inBrackets . intercalate " * " . map show $ factors
>         show (Sin x) = (++) "sin" . inBrackets . show $ x
>         show (Cos x) = (++) "cos" . inBrackets . show $ x
>         show (Tan x) = (++) "tan" . inBrackets . show $ x



> inBrackets :: String -> String
> inBrackets str = "(" ++ str ++ ")"

TODO: Test if string is fully parsed

> parseTerm :: Parsec.ParsecT String () Identity (Term a)
> parseTerm = do
>     spaces
>     term <- try parseFunkWithMultipleArg <|> parseFunkWithOneArg
>     spaces
>     return term


This one consumes a given character (eventually sorounded by spaces) and returns nothing. I use it to parse away separators, for example in parseMul.

> charSeparator :: Char -> Parsec.Parsec String () ()
> charSeparator character = do
>     spaces
>     char character
>     spaces
>     return ()



Funktions with one Argument
---------------------------

> parseFunkWithOneArg :: Parsec.ParsecT String () Identity (Term a)
> parseFunkWithOneArg = parseVariable <|> parseTrig <|> parseBrackets

Brackets are pretty easy (and work as supposed, if they are just treated as functions with 1 argument)

> parseBrackets :: Parsec.Parsec String () (Term a)
> parseBrackets = do
>     char '('
>     spaces
>     term <- parseTerm
>     spaces
>     char ')'
>     return term

Variables
There can be x or y. One day we should think about having x1 and x2 as an alternative.

> parseVariable :: Parsec.Parsec String () (Term a)
> parseVariable = do
>     variable <- (char 'x') <|> char 'y'
>     case variable of
>         'x' -> return VarX
>         'y' -> return VarY
>         _ -> error "keine bekannte Variable"

Trigononmetric functions

> parseTrig :: Parsec.Parsec String () (Term a)
> parseTrig = do
>     trigonometricFunction <- (string "sin") <|> string "cos" <|> string "tan"
>     spaces
>     char '('
>     spaces
>     argument <- parseTerm
>     spaces
>     char ')'
>     case trigonometricFunction of
>         "sin" -> return $ Sin argument
>         "cos" -> return $ Cos argument
>         "tan" -> return $ Tan argument
>         _ -> error "keine trigonometrische Funktion"




Funktion with two Arguments
---------------------------

> parseSub :: Parsec.Parsec String () (Term a)
> parseSub = do
>     minuend <- parseFunkWithOneArg
>     charSeparator '-'
>     subtrahend <- parseFunkWithOneArg
>     return $ Sub minuend subtrahend

> parsePow :: Parsec.Parsec String () (Term a)
> parsePow = do
>     fstArg <- parseFunkWithOneArg
>     charSeparator '^'
>     sndArg <- parseFunkWithOneArg
>     return $ Pow fstArg sndArg



Funktion with multiple Arguments
--------------------------------

Try different parsers. The "try" is important for the case that one parser consumes input and then fails. In this case the try 'resets' the input to the point where nothing was consumed

> parseFunkWithMultipleArg :: Parsec.Parsec String () (Term a)
> parseFunkWithMultipleArg = try parseAdd <|> try parseMul <|> try parsePow <|> try parseSub

Addition

> parseAdd :: Parsec.Parsec String () (Term a)
> parseAdd = do
>     summands <- sepByN 2 parseFunkWithOneArg (charSeparator '+')
>     return $ Add summands

Multiplikation

> parseMul :: Parsec.Parsec String () (Term a)
> parseMul = do
>     factors <- sepByN 2 parseFunkWithOneArg (charSeparator '*')
>     return $ Mul factors

To parse lists of things (that get parsed by parser p), separated by other things (that are parser by parser sep) of length >= n , use sepByN n p sep. It is an mor flexible version of sepBy and sepBy1, infact sepByN 0 = sepBy and sepByN 1 = sepBy1

> sepByN :: (Stream s m t) => Int ->  ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
> sepByN 0 p sep = sepBy p sep
> sepByN 1 p sep = sepBy1 p sep
> sepByN n p sep = do
>     x <- p
>     sep
>     xs <- sepByN (n-1) p sep
>     return (x:xs)










