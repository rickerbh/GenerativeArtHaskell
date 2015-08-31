-- Wave generator

module SineWave where
  
import ArtHelpers
import GlomePerlin

-- JuicyPixels
import Codec.Picture

  -- Rasterific
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import GHC.Float

background = PixelRGBA8 255 255 255 255
lineStroke = PixelRGBA8 20 50 70 255

width = 500
height = 100
xInset = 20
startX = xInset
endX = width - xInset
startY = 50
step = 1

convertToYPoints x = 50 + x * 40
convertToYPoints3 x = 50 + (x ^ 3) * 40
convertToYPointsN x = 50 + (x ^ 3) * (double2Float (SineWave.noise 1 (float2Double x) 100)) * 40

steps = [startX,(startX + step)..endX]
noise x y xinc = perlin (Vec (x / width) (y / height) (1 + xinc))



ys = take (length steps) $ map convertToYPoints (sinGen 0 step)
points = partition 2 1 (zip steps ys)
sineWave = render width height background lineStroke points
               

cys = take (length steps) $ map convertToYPoints (cosGen 0 step)
cpoints = partition 2 1 (zip steps cys)
cosWave = render width height background lineStroke cpoints
               

ys3 = take (length steps) $ map convertToYPoints3 (sinGen 0 step)
points3 = partition 2 1 (zip steps ys3)
sineWave3 = render width height background lineStroke points3


ysN = take (length steps) $ map convertToYPointsN (sinGen 0 step)
pointsN = partition 2 1 (zip steps ysN)
sineWaveN = render width height background lineStroke pointsN
