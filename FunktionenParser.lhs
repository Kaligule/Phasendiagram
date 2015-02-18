> module Calculator.Parser
>     (
>       parseTerm
>     ) where
> 
> import           Control.Applicative  hiding (many, (<|>))
> import           Text.Parsec
> import           Text.Parsec.Expr
> import           Text.Parsec.Language (haskellStyle)
> import qualified Text.Parsec.Token    as P
> import           Text.Parsec.String   (Parser)
> import           Data.Either.Combinators (rightToMaybe)
> import           Data.Maybe (fromJust)

Der Plan ist, erstmal den String in einen Term zu parsen und den dann auszuwerten. Ein Term ist ein Baum, dessen Blätter Konstanten oder Variablen und dessen Knoten Operatoren sind:

> data Term a =
>       Add (Term a) (Term a)
>     | Sub (Term a) (Term a)
>     | Mul (Term a) (Term a)
>     | Div (Term a) (Term a)
>     | Con a
>     | VarX
>     | VarY

Unnoetige Instanzen
-------------------

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

Term waere eine Instanz der Klasse Num, wenn wir ```signum``` oder ```abs``` definieren koennten. Leider geht das nur, wenn wir VarX und VarY kennen.

> --instance (Num a) => Num (Term a)
> --    where
> --        term1 + term2 = Add term1 term2
> --        term1 - term2 = Sub term1 term2
> --        term1 * term2 = Mul term1 term2
> --        fromInteger n = Con (fromInteger n)


Das Auswerten
-------------

> parseStringToFunction :: String -> Maybe (Double -> Double -> Double)
> parseStringToFunction str = fmap evaluateTerm . rightToMaybe . parseTerm $ str


Aber meistens will man einen String zu einem Term machen.

> parseTerm :: String -> Either ParseError (Term Double)
> parseTerm = parse expr "Errorpoint 1"

Wenn man VarX und VarY weiss, kann man auch den ganzen Term berechnen.

> evaluateTerm :: (Num a, Fractional a) => Term a -> a -> a -> a
> evaluateTerm (Add term1 term2) x y = (evaluateTerm term1 x y) + (evaluateTerm term2 x y)
> evaluateTerm (Sub term1 term2) x y = (evaluateTerm term1 x y) - (evaluateTerm term2 x y)
> evaluateTerm (Mul term1 term2) x y = (evaluateTerm term1 x y) * (evaluateTerm term2 x y)
> evaluateTerm (Div term1 term2) x y = (evaluateTerm term1 x y) / (evaluateTerm term2 x y)
> evaluateTerm (Con c)           _ _ = c
> evaluateTerm VarX              x _ = x
> evaluateTerm VarY              _ y = y

Das Parsen selbst
-----------------

Hier kommt das eigentliche Parsen. Davon verstehe ich nicht alles, insbesondere fehlen manche Signaturen.

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
> -- parens :: ParsecT String () Data.Functor.Identity.Identity a
>        -- -> ParsecT String () Data.Functor.Identity.Identity a
> parens = P.parens lexer
> 
> -- constant :: ParsecT String () Data.Functor.Identity.Identity a
>          -- -> ParsecT String () Data.Functor.Identity.Identity a
> constant = P.naturalOrFloat lexer
> 
> -- reservedOp :: ParsecT String () Data.Functor.Identity.Identity a
>            -- -> ParsecT String () Data.Functor.Identity.Identity a
> reservedOp = P.reservedOp lexer
> 
> term =  parens expr
>     <|> (Con . toFloat) <$> constant
>     <?> "term that makes sense"
>     where
>         toFloat :: Either Integer Double -> Double
>         toFloat (Left int) = fromIntegral int
>         toFloat (Right x) = x
> 
> -- binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
> binary name fun = Infix   (do { reservedOp name; return fun })
> 
> expr :: Parser (Term Double)
> expr = buildExpressionParser table term <?> "expression"






