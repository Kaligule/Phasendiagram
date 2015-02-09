TypeModule
==========

I found it usefull to have one Module that imports all the Types that might become usefull in different parts of one project. So this is the Type Module for the Phasendiagram Plotter.

Exports
-------

> module TypeModule (Borders, R2Endomorphism, Vectorfield) where

Imports
-------

Import 2 basic types from Diagrams so we can define a Vectorfield

> import Diagrams.Prelude (P2, R2) 

Borders
-------

Borders of a diagram are writen down with Doubles, using (left, right, bottom, top)

> type Borders = (Double, Double, Double, Double)


The Function all is about
-------------------------

All the Phasendiagram stuff is about getting a feeling for a function and how it acts. The User will be able to choose this function himself. So he will type it into a text field (we get a String). 

After parsing the String  we will have funktion of Type (Double, Double) -> (Double, Double). We will push this one around for some time, so it might be usefull to have a name for it:

> type R2Endomorphism = (Double, Double) -> (Double, Double)

The R2Endomorphism are just a step between Strings and Vectorfields. Vectorfield are more usefull then R2Endomorphisms since they distinguish between Points (P2) and Vectors (R2).

> type Vectorfield = P2 -> R2

Of course R2Endomorphism and Vectorfield could be converted to one another.

