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
> 
> data Term a =
>       Add (Term a) (Term a)
>     | Sub (Term a) (Term a)
>     | Mul (Term a) (Term a)
>     | Div (Term a) (Term a)
>     | Con a
> 
> instance (Show a) => Show (Term a)
>     where
>         show (Add x y) = show x ++ " + " ++ show y
>         show (Sub x y) = show x ++ " - " ++ show y
>         show (Mul x y) = show x ++ " * " ++ show y
>         show (Div x y) = show x ++ " / " ++ show y
>         show (Con x  ) = show x
> 
> 
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
>     <?> "Errorpoint 2"
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
> 
> parseTerm :: String -> Either ParseError (Term Double)
> parseTerm = parse expr "Errorpoint 1"



