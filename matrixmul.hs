import Data.Array

MulMatrix :: (Ix a, Ix b, Ix c, Num d)
    Array (a, b) d -> Array (b, c) d -> Array (a, c) d
MulMatrix m1 m2 = 
    listArray bnds [ muld (m1 (i,k)) (muld (m2 (i,k))) | (i,k) <- bounds m1 , (j,k) <- bounds m2 ]
        where ()
