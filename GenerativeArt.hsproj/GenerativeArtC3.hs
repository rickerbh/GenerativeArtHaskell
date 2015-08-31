module GenerativeArtC3 where

-- 
import ArtHelpers
import Simplex
import GlomePerlin

-- JuicyPixels
import Codec.Picture

  -- Rasterific
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import System.Random

import GHC.Float

simplexGen x y = noise : simplexGen x noise
  where noise = noise2D x y

perlinGen x y xinc = noise * height : perlinGen (x + xinc) (noise * height) xinc
  where noise = perlin (Vec (x / width) (y / height) (1 + xinc))

width = 500
height = 100

background = PixelRGBA8 255 255 255 255
lineStroke = PixelRGBA8 20 50 70 255


line1 = renderDrawing width height background $
          withTexture (uniformTexture lineStroke) $ do
            stroke 5 JoinRound (CapRound, CapRound) $
              line (V2 20 50) (V2 480 50)

seed   = 13561956 :: Int
mygen  = mkStdGen seed

line2 = renderDrawing width height background $
          withTexture (uniformTexture lineStroke) $ do
            stroke 5 JoinRound (CapRound, CapRound) $
              line (V2 20 50) (V2 
                                (fst(randomR (0, width) mygen)) 
                                (fst(randomR (0, height) mygen)))

--Line 3
steps = [0,10..500]
ys = take (length steps) $ randomRs (0, height) mygen
line3Points = partition 2 1 (zip steps ys)

strokes3 = map strokeGen line3Points

line3 = renderDrawing width height background $
          withTexture (uniformTexture lineStroke) $
            mergeStrokes strokes3

-- Line 4
relativeY x = 0.5 * height * x + 50
l4yNoise = map double2Float (take (length steps) $ simplexGen 10 50)
l4ys = map relativeY l4yNoise
line4Points = partition 2 1 (zip steps l4ys)
strokes4 = map strokeGen line4Points

line4 = renderDrawing width height background $
          withTexture (uniformTexture lineStroke) $
            mergeStrokes strokes4

l5ys = map double2Float (perlinGen 10 50 100)
line5Points = partition 2 1 (zip steps l5ys)
strokes5 = map strokeGen line5Points

line5 = renderDrawing width height background $
          withTexture (uniformTexture lineStroke) $
            mergeStrokes strokes5


