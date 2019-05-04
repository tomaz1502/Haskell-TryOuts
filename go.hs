fastpow :: (Num a, Integral a) => a -> a -> a
fastpow b 0 = 1
fastpow b e = if (mod e 2) == 1 then x * x * b else x * x
    where x = fastpow b (div e 2)
