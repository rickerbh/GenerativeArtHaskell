-- Bunch of generated circles/spirals


module CircleArt where

import ArtHelpers
import GlomePerlin

-- JuicyPixels
import Codec.Picture

-- Rasterific
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import GHC.Float
import System.Random

background = PixelRGBA8 255 255 255 255
lineColor = PixelRGBA8 20 50 70 127

width = 500
height = 300
startX = 250
startY = 150
startAngle = 0
endAngle = 360
step = 5
radius = 100

steps = [startAngle,(startAngle + step)..endAngle]

xCalculator centX r angle = centX + (r * cos (radians angle))
yCalculator centY r angle = centY + (r * sin (radians angle))

xs = map (xCalculator 250 radius) steps
ys = map (yCalculator 150 radius) steps

drawPoints = render width height background lineColor


myCircle = drawPoints (partition 2 1 (zip xs ys))





sxGenerator centX r rInc angle angleInc = centX + (r * cos (radians angle)) : sxGenerator centX (r + rInc) rInc (angle + angleInc) angleInc
syGenerator centY r rInc angle angleInc = centY + (r * sin (radians angle)) : syGenerator centY (r + rInc) rInc (angle + angleInc) angleInc

ssteps = [startAngle,(startAngle + step)..1440]
sxs = take (length ssteps) $ sxGenerator 250 10 0.5 startAngle step
sys = take (length ssteps) $ syGenerator 150 10 0.5 startAngle step

mySpiral = drawPoints (partition 2 1 (zip sxs sys))




seed   = 13561956 :: Int
mygen  = mkStdGen seed
radiusNoise = head $ randomRs (0, 10) mygen
radiusNoiseInc = 0.05
getPerlin x y z = double2Float (perlin (Vec (float2Double x) (float2Double y) (float2Double z)))

nsxGenerator centX r rInc rNoise rNoiseInc angle angleInc = centX + (thisR * cos (radians angle)) : nsxGenerator centX (r + rInc) rInc (rNoise + rNoiseInc) rNoiseInc (angle + angleInc) angleInc
  where thisR = r + (getPerlin (r / 500) (angle / 1440) rNoise) * 200 - 100

nsyGenerator centY r rInc rNoise rNoiseInc angle angleInc = centY + (thisR * sin (radians angle)) : nsyGenerator centY (r + rInc) rInc (rNoise + rNoiseInc) rNoiseInc (angle + angleInc) angleInc
  where thisR = r + (getPerlin (r / 300) (angle / 1440) rNoise) * 200 - 100

nssteps = 289
nsxs = take nssteps $ nsxGenerator 250 10 0.5 radiusNoise radiusNoiseInc startAngle step
nsys = take nssteps $ nsyGenerator 150 10 0.5 radiusNoise radiusNoiseInc startAngle step

myNSpiral = drawPoints (partition 2 1 (zip nsxs nsys))



randomNoises = take 100 $ randomRs (0, 10000) mygen

nSpiralGen seed = zip nsxsG nsysG
  where nsxsG = take nssteps $ nsxGenerator 250 10 0.5 seed radiusNoiseInc seed step
        nsysG = take nssteps $ nsyGenerator 150 10 0.5 seed radiusNoiseInc seed step


lotsOSpirals = concat (map (partition 2 1) (map nSpiralGen randomNoises))

crazy100 = render2 width height background lineColor 0.25 lotsOSpirals
