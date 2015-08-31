module GenerativeArtC2 where
  
  -- JuicyPixels
import Codec.Picture

  -- Rasterific
import Graphics.Rasterific
import Graphics.Rasterific.Texture

background = PixelRGBA8 230 230 230 255
drawColor = PixelRGBA8 0 0x86 0xc1 255
strokeColor = PixelRGBA8 130 0 0 255
fillColor = PixelRGBA8 255 255 255 150
black = PixelRGBA8 0 0 0 255
white25 = PixelRGBA8 255 255 255 15

half x = x / 2

width = 500
height = 300
halfWidth = half width
halfHeight = half height

img = renderDrawing width height background $
        withTexture (uniformTexture strokeColor) $ do
          stroke 1 JoinRound (CapRound, CapRound) $
            line (V2 (halfWidth - 70) (halfHeight - 70)) (V2 (halfWidth + 70) (halfHeight + 70))
          stroke 1 JoinRound (CapRound, CapRound) $
            line (V2 (halfWidth + 70) (halfHeight - 70)) (V2 (halfWidth - 70) (halfHeight + 70))
          withTexture (uniformTexture fillColor) $ do
            fill $ circle (V2 halfWidth halfHeight) 22
            withTexture (uniformTexture $ PixelRGBA8 0 0 0 125) $ do
              stroke 6 JoinRound (CapRound, CapRound) $
                circle (V2 halfWidth halfHeight) 25

img2 = renderDrawing width height (PixelRGBA8 180 180 180 255) $
         concentricCircles halfWidth halfHeight 10
                

circleStroke = 1

concentricCircles x y r
  | r <= 200  = texture $ do
                  strokeCircle x y r circleStroke
                  fillTexture $ do
                    fillStrokedCircle x y r circleStroke
                    concentricCircles x y (r + 5)
  | otherwise = texture $ do
                  strokeCircle x y r circleStroke
                  fillTexture $ do
                    fillStrokedCircle x y r circleStroke
  where texture     = withTexture (uniformTexture black)
        fillTexture = withTexture (uniformTexture white25)
  
strokeCircle x y r s = stroke s JoinRound (CapRound, CapRound) $
                         circle (V2 x y) r
fillStrokedCircle x y r s = fill $ circle (V2 x y) (r - (s * 2))
