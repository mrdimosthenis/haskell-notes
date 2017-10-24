module AD_Arithmetic where

findStep :: Integral n => [n] -> n
findStep (x:y:z:_) = let step1 = y - x
                         step2 = z - y
                         step = min (abs step1) (abs step2)
                       in if step1 >= 0 then step else negate step
findStep _ = error "cannot find step"

findMissing :: Integral n => [n] -> n
findMissing lst@(x:_) = let progr = [x, x + findStep lst .. ]
                        in fst $ head $ dropWhile (uncurry (==)) $ zip progr lst
findMissing _ = error "cannot find missing term"
