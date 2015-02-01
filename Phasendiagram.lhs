> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.TwoD.Types
> import Diagrams.TwoD.Path.Metafont
>


> main = mainWith example

> example :: Diagram B R2
> example = bg white $ clipTo (rect (x_2-x_1) (y_2-y_1)) $
>            field 
>         <> (redlines startpoints)

Das hier ist die Funktion, die das Vektorfeld erzeugt

> vectorField :: P2 -> R2
> vectorField point = r2 (sin (y + 1), cos (x + 1))

>   where
>     (x,y) = unp2 point


> arrowAtPoint :: P2 ->  Diagram B R2
> arrowAtPoint point
>   | m == 0    = mempty
>   | otherwise = arrowAt' opts point (sL *^ vf) # alignTL
>   where
>     vf   = vectorField point
>     m    = magnitude $ vectorField point
>
> -- Head size is a function of the length of the vector
> -- as are tail size and shaft length.
>
>     hs   = 0.01* m
>     sW   = 0.0004 * m
>     sL   = 0.005 + 0.1 * m
>     opts = (with & arrowHead  .~ spike
>                  & headLength   .~ Normalized hs
>                  & shaftStyle %~ lwN sW)


> field =  position $ graph arrowAtPoint points

> graph :: (a -> b) -> [a] -> [(a,b)]
> graph f list = zip list (map f list)

Variables for the points that are shown

> detail, x_1, x_2, y_1, y_2 :: Double
> detail = 0.2
> x_1 = -1.0
> x_2 = 1.0
> y_1 = -1.0
> y_2 = 1.0

Create a list of points where the vectors will be placed.

> points :: [P2]
> points = map p2 locs
>   where
>     locs :: [(Double, Double)]
>     locs  = [(x, y) | x <- [x_1, (x_1 + detail) .. x_2], y <- [y_1, (y_1 + detail) .. y_2]]




> -- littlePaths :: [Diagram B R2]
> -- littlePaths = (zipWith ~~ stuetzstellen (tail stuetzstellen))

Wo hin gehe ich vom Startpunkt aus? Und von dort? Und von dort?

> startpoints :: [P2]
> startpoints = map p2 [(x,y) | x <- [-0.1,0.1], y  <- [-0.1,0.1]]
> 
> epsilon :: Double
> epsilon = 0.001

> stuetzstellen :: P2 -> [P2]
> stuetzstellen startpoint = take 10000 $ iterate (\p -> translate (scale epsilon (vectorField p)) p) startpoint


> redlines :: [P2] ->  Diagram B R2
> redlines startpoints = foldl (<>) mempty ( map (lc red . cubicSpline False .stuetzstellen) startpoints)
 
