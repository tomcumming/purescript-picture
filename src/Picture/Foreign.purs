module Picture.Foreign where

import Prelude

import Effect (Effect)
import Picture.Path (Path)
import Picture.Picture as P
import Picture.Transform (Transform)

newtype FillStyle = FillStyle String
type Handle = Effect Unit

foreign import data HTMLCanvasElement :: Type
foreign import data Context2D :: Type

foreign import onResize
  :: { canvas :: HTMLCanvasElement, action :: Effect Unit }
  -> Effect Handle

foreign import onAnimationFrame
  :: Effect Unit
  -> Effect Unit

foreign import getDimensions
  :: HTMLCanvasElement
  -> Effect { width :: Number, height :: Number }

foreign import setCanvasPixels
  :: { width :: Number, height :: Number, canvas :: HTMLCanvasElement }
  -> Effect Unit

foreign import getContext
  :: HTMLCanvasElement -> Effect Context2D

foreign import setTransform
  :: { ctx :: Context2D, tx :: Transform } -> Effect Unit

foreign import setFillStyle
  :: { ctx :: Context2D, style :: FillStyle } -> Effect Unit

foreign import fillPoly
  :: { ctx :: Context2D, path :: Path } -> Effect Unit

fillStyle :: P.Color -> FillStyle
fillStyle = FillStyle <<< case _ of
  P.ColorName c -> c
  P.RGBA r g b a -> "rgba(" <> s r <> " " <> s g <> " " <> s b <> " / " <> show a <> ")"
  where
  s = (_ * 255.0) >>> show
