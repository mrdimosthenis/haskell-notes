module AY_CommonDenominators where

type Ratio a = (a, a)

changeDenom :: Integral a => a -> Ratio a -> Ratio a
changeDenom m (numer, denom) = (numer * fact, denom * fact)
  where fact = m `div` denom

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs xs = map (changeDenom mult) xs
  where mult = foldl1 lcm $ map snd xs
