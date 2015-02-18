Phasendiagram
=============

Compilerflags (wharscheinlich zu viele) und Importe (wahrscheinlich zu viele)

> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
>

> import TypeModule (Borders, R2Endomorphism, Vectorfield)




> clipToBorders :: Borders -> Diagram B R2 -> Diagram B R2
> clipToBorders (x1,x2,y1,y2) = clipTo (rect (x2-x1) (y2-y1)) 

Die Main von mainWith ist ganz cool. Ich muss einmal compilieren (mit ```ghc --make Phasendiagram.lhs```) und dann einmal ausfuehren mit ```./Phasendiagram -o Phasendiagram.svg -l -s Phasendiagram.lhs -h 500```. Dann wird das Bild erstellt und nach jeder gespeicherten Aenderung neu erstellt.

> main :: IO() 
> main = do
>   borders <- getBorders
>   function <- getFunction
>   let vectorField = prepareVectorfield function
>   mainWith (phasediagram borders vectorField)

> phasediagram :: Borders -> Vectorfield -> Diagram B R2
> phasediagram borders vectorField = bg white
>              . clipToBorders borders
>              $ field borders vectorField
>              <> (redlines vectorField startpoints)


Die Variablen
-------------

Variablen sind hier noch fest vercodet, spaeter sollen sie natuerlich vom User eingegeben werden.

The Area that is shown is between this choordinates

> getBorders :: IO Borders
> getBorders = return (-2,2,-2,2)

The vectorField that is shown

> getFunction :: IO R2Endomorphism
> getFunction = return (\(x,y) -> (x+y, x))



Die Differentialgleichung, die wir visualisieren, ist explizit und zeitunabhaengig. Sie hat also die Form ```x'(t) = f(x(t))```.
(Irgendwann koennte man eventuell auch zeitabhaengige Diffgleichungen anschauen. Dazu würde man nicht nur ein epsilon haben, sondern ein epsilon_raum und ein epsilon_zeit).
Die Funktion f heißt bei uns Vektorfeld. Sie bildet einen Punkt im Raum (wir haben nur 2 Dimensionen, damit man die ganze Geschichte auch zeichnen kann) auf einen Vektor ab.

> prepareVectorfield :: R2Endomorphism -> Vectorfield
> prepareVectorfield f = r2 . f . unp2

Das Vektorfeld
--------------

Liefert eine Visualisierung des Vektorfeldes vectorField fuer einen Punkt.

> arrowAtPoint :: Vectorfield -> P2 -> Diagram B R2
> arrowAtPoint vectorField point = arrowDia (vectorField point)-- # showOrigin

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

> field :: Borders -> Vectorfield ->  Diagram B R2
> field borders vectorfield =  position $ graph (arrowAtPoint vectorfield) (points borders)
>   where
>     graph :: (a -> b) -> [a] -> [(a,b)]
>     graph f list = zip list (map f list)





Create a list of points where the vectors will be placed.

> points :: Borders -> [P2]
> points borders = map p2 locs
>   where
>     locs :: [(Double, Double)]
>     locs  = [(x, y) | x <- [x1, (x1 + detail) .. x2], y <- [y1, (y1 + detail) .. y2]]
>     
>     (x1,x2,y1,y2) = borders

The distance between the arrows (perhaps this could be calculated from the borders?

>     detail :: Double
>     detail = 0.2


Die roten Linien
----------------

Einige Punkte (zum Beispiel die Randbedingungen der Differentialgleichung) werden ausgewählt, um als Startpunkt der roten Linien zu dienen.

> startpoints :: [P2]
> startpoints = map p2 [(x,y) | x <- [-0.5,0.5], y  <- [-0.5,0.5]]

Bin ich an einem Punkt, schauen wir den entsprechenden Vektor des Vektorfeldes an, multipliziere ihn mit epsilon ...

> epsilon :: Double
> epsilon = 0.002

und bewege uns um genau diesen Vektor weiter. Dann beginnen wir von vorn. Dadurch erhalte ich (pro Startpunkt) eine Reihe von Punkten, welche auf einer Kurve liegen.

> stuetzstellen :: Vectorfield ->  P2 -> [P2]
> stuetzstellen vectorField startpoint = take 500 $ iterate (\p -> translate (scale epsilon (vectorField p)) p) startpoint

Diese Kurve kann durch den Befehl ```cubicSpline``` interpolliert werden. Dabei werden nicht einfach grob die Punkte verbunden sondern weiche Linien dazwischen gezeichnet. (Ich glaube nicht, dass es Bezierkurven sind, aber sie sehen genauso schoen aus.

```redline``` mal also die Spur des Vektorfeldes für einen Anfangspunkt,

> redline :: Vectorfield ->  P2 -> Diagram B R2
> redline vectorField = lc red . cubicSpline False . stuetzstellen vectorField

```redlines``` tut das gleiche fuer mehrere Punkte.

> redlines :: Vectorfield -> [P2] ->  Diagram B R2
> redlines vectorField startpoints = foldl (<>) mempty (map (redline vectorField) startpoints)
 
