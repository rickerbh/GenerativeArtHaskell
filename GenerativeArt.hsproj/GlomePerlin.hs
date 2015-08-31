-- Perlin implementation lifted from Glome https://wiki.haskell.org/Glome

{-# LANGUAGE BangPatterns #-}

module GlomePerlin where
  
import Data.Array.IArray

type Flt = Double

-- | 3d type represented as a record of unboxed floats.
data Vec = Vec !Flt !Flt !Flt deriving Show

vec :: Flt -> Flt -> Flt -> Vec
vec !x !y !z = (Vec x y z)

iabs :: Int -> Int
iabs !a =
 if a < 0 then (-a) else a

fabs :: Flt -> Flt
fabs !a = 
 if a < 0 then (-a) else a

vdot :: Vec -> Vec -> Flt
vdot !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 (x1*x2)+(y1*y2)+(z1*z2)

vlen :: Vec -> Flt
vlen !v1 = sqrt (vdot v1 v1)

phi :: Array Int Int
phi = listArray (0,11) [3,0,2,7,4,1,5,11,8,10,9,6]

gamma :: Int -> Int -> Int -> Vec
gamma i j k =
 let a = phi!(mod (iabs k) 12)
     b = phi!(mod (iabs (j+a)) 12)
     c = phi!(mod (iabs (i+b)) 12)
 in grad!c

grad :: Array Int Vec
grad = listArray (0,11) 
         $ filter (\x -> let l = vlen x in l < 1.5 && l > 1.1) 
                  [Vec x y z | x <- [(-1),0,1],
                               y <- [(-1),0,1],
                               z <- [(-1),0,1]] 

omega :: Flt -> Flt
omega t_ = 
 let t     = fabs t_
     tsqr  = t*t
     tcube = tsqr*t
 in (-6)*tcube*tsqr + 15*tcube*t - 10*tcube + 1

knot :: Int -> Int -> Int -> Vec -> Flt
knot i j k v =
 let Vec x y z = v
 in (omega x) * (omega y) * (omega z) * (vdot (gamma i j k) v)

noise :: Vec -> Flt 
noise (Vec x y z) =
 let i = floor x
     j = floor y
     k = floor z
     u = x-(fromIntegral i)
     v = y-(fromIntegral j)
     w = z-(fromIntegral k)
 in knot i j k             (Vec u v w) +
    knot (i+1) j k         (Vec (u-1) v w) +
    knot i (j+1) k         (Vec u (v-1) w) +
    knot i j (k+1)         (Vec u v (w-1)) +
    knot (i+1) (j+1) k     (Vec (u-1) (v-1) w) +
    knot (i+1) j (k+1)     (Vec (u-1) v (w-1)) +
    knot i (j+1) (k+1)     (Vec u (v-1) (w-1)) +
    knot (i+1) (j+1) (k+1) (Vec (u-1) (v-1) (w-1))

perlin :: Vec -> Flt
perlin v =
 let p = ((noise v)+1)*0.5
 in if p > 1 
    then error $ "perlin noise error, 1 < " ++ (show p)
    else if p < 0 
         then error $ "perlin noise error, 0 > " ++ (show p)
         else p

