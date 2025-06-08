aux :: Double -> Double -> Double -> Double -> Double
aux a eps h h_old| (h - h_old)*(h - h_old) <= eps*eps = h
                 | otherwise = aux a eps ((h+a/h)*0.5) h

wurzel a eps = aux a eps ((1+a)*0.5) ((1+a)*0.5+2*eps)

aux2 :: Double -> Double -> Double -> Double
aux2 a eps h | (h_new - h)*(h_new - h) <= eps*eps = h_new
             | otherwise = aux2 a eps h_new
               where h_new = ((h+a/h)*0.5)

wurzel2 a eps = aux2 a eps ((1+a)*0.5)
