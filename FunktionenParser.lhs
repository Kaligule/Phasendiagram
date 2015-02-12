
Funktionen parsen
=================

Wir wollen einen Term parsen, der Variablen 'x' und 'y' enthaelt.
Dabei probieren wir gleich mal aus, was wir aus diesem Tutorial lernen koennen: http://unbui.lt/#!/post/haskell-parsec-basics

Parsec soll die meiste Arbeit fuer uns machen

> import Text.Parsec as Parsec
> import Text.Parsec ((<?>), spaces, string, char, try)
> -- import Control.Applicative
> import Control.Monad.Identity (Identity)

> par rule text = Parsec.parse rule "Funktionenstring" text

> main :: IO()
> main = do
>     let stringToParse = "x*sin (y)*x"
>     -- let termParseResult = (parseTerm stringToParse) -- :: Term Double
>     let termParseResult = (par parseTerm stringToParse) :: Either ParseError (Term Double) 
>     case termParseResult of
>         Right term -> putStrLn $ "Success: " ++ show term
>         Left errormessage -> print errormessage


Wir wollen einfache Funktionen mit Num-Argumenten parsen.

> data Term a = Con a           -- some constant 
>             | VarX            -- Variable x
>             | VarY            -- Variable y
>             | Add (Term a) (Term a) -- addieren
>             | Sub (Term a) (Term a) -- suptrahieren
>             | Mul (Term a) (Term a) -- multiplizieren
>             | Sin (Term a)          -- sinus
>             | Cos (Term a)          -- cosinus
>             | Tan (Term a)          -- tangens


Kann man aufschreiben, wird man aber spaeter nicht brauchen.

> instance (Show a) => Show (Term a)
>     where
>         show (Con a) = show a
>         show VarX = "x"
>         show VarY = "y"
>         show (Add x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
>         show (Sub x y) = "(" ++ show x ++ "-" ++ show y ++ ")"
>         show (Mul x y) = "(" ++ show x ++ "*" ++ show y ++ ")"
>         show (Sin x) = "sin(" ++ show x ++ ")"
>         show (Cos x) = "cos(" ++ show x ++ ")"
>         show (Tan x) = "tan(" ++ show x ++ ")"


> parseTerm :: Parsec.ParsecT String () Identity (Term a)
> parseTerm = do
>     spaces
>     term <- try parseFunkWithTwoArg <|> parseFunkWithOneArg
>     spaces
>     return term












Funktion with two Arguments
---------------------------

> parseFunkWithTwoArg :: Parsec.Parsec String () (Term a)
> parseFunkWithTwoArg = try parseAdd <|> parseMul

Addition (and subtraction)

> parseAdd :: Parsec.Parsec String () (Term a)
> parseAdd = do
>     fstArg <- parseFunkWithOneArg
>     spaces
>     operator <- char '+' <|> char '-'
>     spaces
>     sndArg <- parseFunkWithOneArg
>     case operator of
>         '+' -> return $ Add fstArg sndArg
>         '-' -> return $ Sub fstArg sndArg
>         _ -> error "not a know funktion with two arguments"

> parseMul :: Parsec.Parsec String () (Term a)
> parseMul = do
>     fstArg <- parseFunkWithOneArg
>     spaces
>     operator <- char '*'
>     spaces
>     sndArg <- parseFunkWithOneArg
>     return $ Mul fstArg sndArg



Funktions with one Argument
---------------------------

> parseFunkWithOneArg :: Parsec.ParsecT String () Identity (Term a)
> parseFunkWithOneArg = parseVariable <|> parseTrig <|> parseBrackets

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

> parseBrackets :: Parsec.Parsec String () (Term a)
> parseBrackets = do
>     char '('
>     spaces
>     term <- parseTerm
>     spaces
>     char ')'
>     return term



