> module FunktionenParser ( parseR1toR1
>                         , parseR2toR1
>                         , parseR3toR1
>                         , parseR1toR2
>                         , parseR2toR2
>                         , parseR3toR2
>                         ) where
> 
> import           Control.Applicative  hiding (many, (<|>))
> import           Text.Parsec
> import           Text.Parsec.Expr
> import           Text.Parsec.Language (haskellStyle)
> import qualified Text.Parsec.Token    as P
> import           Text.Parsec.String   (Parser)
> import           Data.Either.Combinators (rightToMaybe)
> import           Data.Maybe (fromJust)

Exporte
-------

Um diese Funktionen geht es letztendlich.

> parseR1toR1 :: String -> Maybe (Double -> Double)
> parseR1toR1 = fmap evalTerm1 . rightToMaybe . parse parse1Term "Funktion from R1 to R1"
> parseR2toR1 :: String -> Maybe ((Double, Double) -> Double)
> parseR2toR1 = fmap evalTerm2 . rightToMaybe . parse parse1Term "Funktion from R2 to R1"
> parseR3toR1 :: String -> Maybe ((Double, Double, Double) -> Double)
> parseR3toR1 = fmap evalTerm3 . rightToMaybe . parse parse1Term "Funktion from R3 to R1"

Leider ist es sehr viel schwieriger, die Funktionen in den R^2 richtig hin zu bekommen. Man braucht zwei Hilfsfunktionen.

> parseR1toR2 :: String -> Maybe (Double -> (Double,Double))
> parseR1toR2 = evalHelper evalTerm1 . rightToMaybe . parse parse2Term "Funktion from R1 to R2"
> parseR2toR2 :: String -> Maybe ((Double, Double) -> (Double, Double))
> parseR2toR2 = evalHelper evalTerm2 . rightToMaybe . parse parse2Term "Funktion from R2 to R2"
> parseR3toR2 :: String -> Maybe ((Double, Double, Double) -> (Double, Double))
> parseR3toR2 = evalHelper evalTerm3 . rightToMaybe . parse parse2Term "Funktion from R3 to R2"

> evalHelper ::(term -> argumente -> ergebnis) -> Maybe (term,term) -> Maybe (argumente -> (ergebnis, ergebnis))
> evalHelper = fmap . twoStarts1
> twoStarts1 :: (a -> b -> c) -> (a,a) -> b -> (c,c)
> twoStarts1 f as b = mapDouble (\a -> f a b) as

> mapDouble :: (a -> b) -> (a,a) -> (b,b)
> mapDouble f (x,y) = (f x, f y)


Das Aussenrum parsen
--------------------

Die eigentliche Arbeit wird von dieser Funktion erledigt.

> parseTerm :: String -> Either ParseError (Term Double)
> parseTerm = parse expr "parse String to a Term (Double)."

Um Tuple zu parsen benutzt man diese Funktionen. Die benutzen ```expr``` und kuemmern sich um die Klammern.

> parse1Term :: Parser (Term Double)
> parse1Term = expr
> parse2Term :: Parser (Term Double, Term Double)
> parse2Term = do
>     char '('
>     term1 <- expr
>     char ','
>     term2 <- expr
>     char ')'
>     return (term1, term2)
> parse3Term :: Parser (Term Double, Term Double, Term Double)
> parse3Term = do
>     char '('
>     term1 <- expr
>     char ','
>     term2 <- expr
>     char ','
>     term3 <- expr
>     char ')'
>     return (term1, term2, term3)


Das Auswerten
-------------

> evalTerm0 :: (Num a, Fractional a) => Term a -> a
> evalTerm0 term           = evaluateTerm term (undefined, undefined, undefined)
> evalTerm1 :: (Num a, Fractional a) => Term a -> a -> a
> evalTerm1 term x         = evaluateTerm term (x, undefined, undefined)
> evalTerm2 :: (Num a, Fractional a) => Term a -> (a, a) -> a
> evalTerm2 term (x, y)    = evaluateTerm term (x, y, undefined)
> evalTerm3 :: (Num a, Fractional a) => Term a -> (a, a, a) -> a
> evalTerm3 term (x, y, z) = evaluateTerm term (x, y, z)



Wenn man VarX, VarY und VarZ weiss, kann man auch den ganzen Term berechnen.

> evaluateTerm :: (Num a, Fractional a) => Term a -> (a, a, a) -> a
> evaluateTerm (Add term1 term2) args = (evaluateTerm term1 args) + (evaluateTerm term2 args)
> evaluateTerm (Sub term1 term2) args = (evaluateTerm term1 args) - (evaluateTerm term2 args)
> evaluateTerm (Mul term1 term2) args = (evaluateTerm term1 args) * (evaluateTerm term2 args)
> evaluateTerm (Div term1 term2) args = (evaluateTerm term1 args) / (evaluateTerm term2 args)
> evaluateTerm (Con c) _      = c
> evaluateTerm VarX (x, _, _) = x
> evaluateTerm VarY (_, y, _) = y
> evaluateTerm VarZ (_, _, z) = z


Die eigentliche Arbeit
======================


Der Datentyp Term ...
---------------------

Der Plan ist, erstmal den String in einen Term zu parsen und den dann auszuwerten. Ein Term ist ein Baum, dessen Blätter Konstanten oder Variablen und dessen Knoten Operatoren sind:

> data Term a = Add (Term a) (Term a)
>             | Sub (Term a) (Term a)
>             | Mul (Term a) (Term a)
>             | Div (Term a) (Term a)
>             | Con a
>             | VarX
>             | VarY
>             | VarZ

...und seine unnoetigen Instanzen
---------------------------------

Terme sind instancen von einigen Klassen. Ich weiss nicht, ob ich die mal brauche, aber es fuehlt sich falsch an, diese Instanzen nicht zu deklarieren.

Term ist eine Instanz der Klasse Show.
 
> instance (Show a) => Show (Term a)
>     where
>         show (Add x y) = show x ++ " + " ++ show y
>         show (Sub x y) = show x ++ " - " ++ show y
>         show (Mul x y) = show x ++ " * " ++ show y
>         show (Div x y) = show x ++ " / " ++ show y
>         show (Con x  ) = show x
>         show VarX      = "x"
>         show VarY      = "y"
>         show VarZ      = "z"

Term waere eine Instanz der Klasse Num, wenn wir ```signum``` oder ```abs``` definieren koennten. Leider geht das nur, wenn wir VarX und VarY kennen.

> --instance (Num a) => Num (Term a)
> --    where
> --        term1 + term2 = Add term1 term2
> --        term1 - term2 = Sub term1 term2
> --        term1 * term2 = Mul term1 term2
> --        fromInteger n = Con (fromInteger n)






Das Parsen selbst
-----------------

Hier kommt das eigentliche Parsen. Davon verstehe ich nicht alles, insbesondere fehlen die Signaturen von ```table``` und ```binary```.

```expr``` ist der letztendliche Parser

> expr :: Parser (Term Double)
> expr = buildExpressionParser table term <?> "expression"

```term``` verstehe ich nicht wirklich

> term :: Parser (Term Double)
> term =  parens expr
>     <|> constant
>     <|> parseVariable
>     <?> "term that makes sense"

Das hier ist die Tabelle von Operatoren, die geparsed werden koennen.

> table = [ [binary "*" Mul AssocLeft, binary "/" Div AssocLeft ]
>         , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft ]
>         ]
> 
> 
> lexer :: P.TokenParser ()
> lexer = P.makeTokenParser (haskellStyle
>                           { P.reservedOpNames = ["+", "-", "*", "/"]
>                           }
>                           )
>


Mit parens kann Sachen auch dann parsen, wenn sie in Klammern stehen

> parens :: Parser (Term a) -> Parser (Term a)
> parens = P.parens lexer

Eine Konstante parsen ist nicht trivial, weil man nicht weiss ob sie zum Schluss mit 3 oder mit 3.0 da stehen wird. Schwierig. Deshalb parsed man zuerst mit P.naturalOrFloat beide Moeglichkeiten und wandeld danach die Integer wieder in Double um.
 
> constant :: Parser (Term Double)
> constant = Con <$> toFloat <$> P.naturalOrFloat lexer
>     where
>         toFloat :: Either Integer Double -> Double
>         toFloat (Left int) = fromIntegral int
>         toFloat (Right x) = x
> 

Eine Variable parsen ist eigentlich leicht: Wenn x da steht nimmt man VarX, wenn Y da steht nimmt man varY. Bei VarZ genauso. Man  koennte sich jetzt ueberlegen, ob man auch alternative bezeichnungen zulaesst, zum Beispiel X, Y, Z,  oder x1, x2, x3.

> parseVariable :: Parser (Term a)
> parseVariable = do
>     variable <- (char 'x') <|> char 'y' <|> char 'z'
>     case variable of
>         'x' -> return VarX
>         'y' -> return VarY
>         'z' -> return VarZ
>         _   -> error "keine bekannte Variable"
> 




> -- binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
> binary name fun = Infix (helper name fun)
>
> helper :: String -> (a -> a -> a) -> Parser (a -> a -> a)
> helper name fun = do
>     reservedOp name
>     return fun
> 


> reservedOp :: String -> Parser ()
> reservedOp = P.reservedOp lexer
