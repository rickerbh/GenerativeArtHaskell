-- A collection of helper functions that are intended to be shared
-- Not all modules use these functions, but they probably should.

module ArtHelpers where
  
import GlomePerlin

-- JuicyPixels
import Codec.Picture

-- Rasterific
import Graphics.Rasterific
import Graphics.Rasterific.Texture

-- System
import Data.List.Split
import GHC.Float

-- Basic geometry helpers
degreesToRadians angle = angle * pi / 180
radiansToDegrees angle = angle * 180 / pi
radians = degreesToRadians

-- Partition an array into groups of size n, stepping 
-- through the lst step items at a time
partition n step lst = 
  chop (\xs -> (take n xs, drop step xs) ) lst

-- Merges a number of strokes together for a do block
mergeStrokes [] = error "Need to provide at least 1 item"
mergeStrokes [x] = x
mergeStrokes (x:xs) = x >> mergeStrokes xs

-- Generators
sinGen deg inc = sin (radians deg) : sinGen (deg + inc) inc
cosGen deg inc = cos (radians deg) : cosGen (deg + inc) inc

-- Drawing helpers
strokeGen xs = strokeGenW 5 xs
strokeGenW w xs = stroke w JoinRound (CapRound, CapRound) $
                 line (V2 (fst t1) (snd t1)) (V2 (fst t2) (snd t2))
  where t1 = head xs
        t2 = head (reverse xs)

render w h bgColor lineColor points = renderDrawing w h bgColor $
                                            withTexture (uniformTexture lineColor) $
                                              mergeStrokes strokes
  where strokes = map strokeGen points

render2 w h bgColor lineColor strokeW points = renderDrawing w h bgColor $
                                                 withTexture (uniformTexture lineColor) $
                                                   mergeStrokes strokes
  where strokes = map (strokeGenW strokeW) points
  