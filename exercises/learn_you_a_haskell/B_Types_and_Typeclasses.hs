module B_Types_and_Typeclasses where

data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)

firstColour :: Colour
firstColour = minBound :: Colour

reverseColourOrder :: [Colour]
reverseColourOrder = reverse [minBound :: Colour .. maxBound :: Colour]

paintMix :: Colour -> Colour -> Colour
paintMix c1 c2 = [c1 .. c2] !! quot (length [c1 .. c2]) 2
