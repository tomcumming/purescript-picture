module Picture
  ( picture
  , testPicture
  ) where

import Prelude

import Data.Number (pi)
import Effect (Effect)
import Picture.Foreign (Context2D, setFillStyle, setTransform)
import Picture.Foreign as Fn
import Picture.Path as Path
import Picture.Picture (Action(..), visitPicture)
import Picture.Picture as P

picture
  :: P.Color
  -> (Number -> Number -> P.Picture)
  -> Fn.HTMLCanvasElement
  -> Effect Unit
picture clearCol pf canvas = do
  ctx <- Fn.getContext canvas
  let sizeAndDraw = do
        { width, height } <- Fn.getDimensions canvas
        Fn.setCanvasPixels { width, height, canvas }
        redraw clearCol pf canvas ctx
  _handle <- Fn.onResize { canvas, action: Fn.onAnimationFrame sizeAndDraw }
  sizeAndDraw

redraw
  :: P.Color
  -> (Number -> Number -> P.Picture)
  -> Fn.HTMLCanvasElement
  -> Fn.Context2D
  -> Effect Unit
redraw clearCol pf canvas ctx = do
  { width, height } <- Fn.getDimensions canvas
  setTransform { ctx, tx: one }
  clearScreen width height clearCol ctx
  setFillStyle { ctx, style: Fn.fillStyle (P.ColorName "black") }
  visitPicture
    (pictureAction ctx)
    one
    (pf width height)

clearScreen :: Number -> Number -> P.Color -> Context2D -> Effect Unit
clearScreen w h bg ctx = do
  setFillStyle { ctx, style: Fn.fillStyle bg }
  let path = Path.absoluteRect 0.0 0.0 w h
  Fn.fillPoly { ctx, path }

pictureAction :: Fn.Context2D -> P.Action -> Effect Unit
pictureAction ctx = case _ of
  SetTransform tx -> Fn.setTransform { ctx, tx }
  Fill path -> Fn.fillPoly { ctx, path }

testPicture :: Fn.HTMLCanvasElement -> Effect Unit
testPicture = picture (P.rgb 1.0 0.5 0.5) go
  where
    go :: Number -> Number -> P.Picture
    go width height = P.screenSpace width height
      $ P.rotate (pi / 8.0)
      $ P.fill Path.rect
