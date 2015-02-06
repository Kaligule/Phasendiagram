Phasendiagram
=============

Compilerflags (wharscheinlich zu viele) und Importe (wahrscheinlich zu viele)

> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
>

Die Main von mainWith ist ganz cool. Ich muss einmal compilieren (mit ```ghc --make Phasendiagram.lhs```) und dann einmal ausfuehren mit ```./Phasendiagram -o Phasendiagram.svg -l -s Phasendiagram.lhs -h 500```. Dann wird das Bild erstellt und nach jeder gespeicherten Aenderung neu erstellt.

> main :: IO()
> main = do
>   mainWith phasediagram

> phasediagram :: Diagram B R2
> phasediagram = (bg white . clipTo (rect (x_2-x_1) (y_2-y_1))) $
>                    field 
>                    <> (redlines startpoints)

Die Differentialgleichung, die wir visualisieren, ist explizit und zeitunabhaengig. Sie hat also die Form ```x'(t) = f(x(t))```.
(Irgendwann koennte man eventuell auch zeitabhaengige Diffgleichungen anschauen. Dazu würde man nicht nur ein epsilon haben, sondern ein epsilon_raum und ein epsilon_zeit).
Die Funktion f heißt bei uns Vektorfeld. Sie bildet einen Punkt im Raum (wir haben nur 2 Dimensionen, damit man die ganze Geschichte auch zeichnen kann) auf einen Vektor ab.


> vectorField :: P2 -> R2
> vectorField point = r2 (y,sin x)
>   where
>     (x,y) = unp2 point

Das Vektorfeld
--------------

Liefert eine Visualisierung des Vektorfeldes vectorField fuer einen Punkt.

> arrowAtPoint :: P2 -> Diagram B R2
> arrowAtPoint point = arrowDia (vectorField point)

Ein Vektorpfeil wird gezeichnet. Das Bild haengt tatsaechlich nur vom Vektor ab (nicht von dem Punkt, wo der Pfeil gemalt wird) und hat seinen Ursprung im hintersten Punkt des Schaftes.

> arrowDia :: R2 -> Diagram B R2
> arrowDia vec
>   | m  == 0   = mempty
>   | otherwise = arrowV' opts (scale sL vec)
>   where
>     m    = magnitude vec
>
> -- Head size is a function of the length of the vector
> -- as are tail size and shaft length.
>
>     hs   = 0.005 + 0.0150 * log (1+m)
>     sW   = 0.001 + 0.0008 * log (1+m)
>     sL   = 0.080 + 0.0200 * log (1+m)
>     opts = (with & arrowHead  .~ dart -- spike is nice, too
>                  & headLength .~ Normalized hs
>                  & shaftStyle %~ lwN sW)

Ein Bild von allen Pfeilen an ihrem richtigen Platz

> field =  position $ graph arrowAtPoint points
>   where
>     graph :: (a -> b) -> [a] -> [(a,b)]
>     graph f list = zip list (map f list)



The area that is shown is between this choordinates

> x_1, x_2, y_1, y_2 :: Double
> x_1 = -2.0
> x_2 = 2.0
> y_1 = -2.0
> y_2 = 2.0

The distance between the arrows

> detail :: Double
> detail = 0.5

Create a list of points where the vectors will be placed.

> points :: [P2]
> points = map p2 locs
>   where
>     locs :: [(Double, Double)]
>     locs  = [(x, y) | x <- [x_1, (x_1 + detail) .. x_2], y <- [y_1, (y_1 + detail) .. y_2]]


Die roten Linien
----------------

Einige Punkte (zum Beispiel die Randbedingungen der Differentialgleichung) werden ausgewählt, um als Startpunkt der roten Linien zu dienen.

> startpoints :: [P2]
> startpoints = map p2 [(x,y) | x <- [-0.1,0.1], y  <- [-0.1,0.1]]

Bin ich an einem Punkt, schauen wir den entsprechenden Vektor des Vektorfeldes an, multipliziere ihn mit epsilon ...

> epsilon :: Double
> epsilon = 0.002

und bewege uns um genau diesen Vektor weiter. Dann beginnen wir von vorn. Dadurch erhalte ich (pro Startpunkt) eine Reihe von Punkten, welche auf einer Kurve liegen.

> stuetzstellen :: P2 -> [P2]
> stuetzstellen startpoint = take 5000 $ iterate (\p -> translate (scale epsilon (vectorField p)) p) startpoint

Diese Kurve kann durch den Befehl ```cubicSpline``` interpolliert werden. Dabei werden nicht einfach grob die Punkte verbunden sondern weiche Linien dazwischen gezeichnet. (Ich glaube nicht, dass es Bezierkurven sind, aber sie sehen genauso schoen aus.

```redline``` mal also die Spur des Vektorfeldes für einen Anfangspunkt,

> redline :: P2 -> Diagram B R2
> redline = lc red . cubicSpline False . stuetzstellen

```redlines``` tut das gleiche fuer mehrere Punkte.

> redlines :: [P2] ->  Diagram B R2
> redlines startpoints = foldl (<>) mempty (map redline startpoints)
 
