
Funktionen parsen
=================

Wir wollen einen Term parsen, der Variablen 'x' und 'y' enthaelt.
Dabei probieren wir gleich mal aus, was wir aus diesem Tutorial lernen koennen: http://unbui.lt/#!/post/haskell-parsec-basics

Parsec soll die meiste Arbeit fuer uns machen

> import Text.Parsec as Parsec
> import Text.Parsec ((<?>))
> -- import Control.Applicative
> import Control.Monad.Identity (Identity)

> par rule text = Parsec.parse rule "Funktionenstring" text

> main :: IO()
> main = do
>     let stringToParse = "sin ( cos (x))"
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
>             | Sin (Term a)
>             | Cos (Term a)
>             | Tan (Term a)

Kann man aufschreiben, wird man aber spaeter nicht brauchen.

> instance (Show a) => Show (Term a)
>     where
>         show (Con a) = show a
>         show VarX = "xVariable"
>         show VarY = "yVariable"
>         show (Add x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
>         show (Sub x y) = "(" ++ show x ++ "-" ++ show y ++ ")"
>         show (Sin x) = "sin(" ++ show x ++ ")"
>         show (Cos x) = "cos(" ++ show x ++ ")"
>         show (Tan x) = "tan(" ++ show x ++ ")"

Parse an Term (should be improved)

> parseTerm :: Parsec.ParsecT String () Identity (Term a)
> parseTerm = parseVariable <|> parseTrig

> parseVariable :: Parsec.Parsec String () (Term a)
> parseVariable = do
>     x <- (Parsec.char 'x') <|> Parsec.char 'y'
>     case x of
>         'x' -> return VarX
>         'y' -> return VarY
>         _ -> error "keine bekannte Variable"

Trigononmetric functions

> parseTrig :: Parsec.Parsec String () (Term a)
> parseTrig = do
>     trigonometricFunction <- (Parsec.string "sin") <|> Parsec.string "cos" <|> Parsec.string "tan"
>     Parsec.spaces
>     Parsec.char '('
>     Parsec.spaces
>     argument <- parseTerm
>     Parsec.spaces
>     Parsec.char ')'
>     case trigonometricFunction of
>         "sin" -> return $ Sin argument
>         "cos" -> return $ Cos argument
>         "tan" -> return $ Tan argument
>         _ -> error "keine trigonometrische Funktion"
