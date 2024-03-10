module Picture.Picture
  ( Color(..)
  , rgb
  , Picture
  , fill
  , scale
  , translate
  , rotate
  , screenSpaceBounds
  , screenSpace
  , Action(..)
  , visitPicture
  ) where

import Prelude

import Control.Monad.State (StateT, evalStateT, get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Picture.Path (Path)
import Picture.Transform as TX
import Unsafe.Reference (unsafeRefEq)

data Color
  = RGBA Number Number Number Number
  | ColorName String

rgb :: Number -> Number -> Number -> Color
rgb r g b = RGBA r g b 1.0

newtype Picture = Picture (Unit -> Picture')

lazy :: Picture' -> Picture
lazy p = Picture $ \_unit -> p

data Picture'
  = Blank
  -- | These should be right leaning for performance
  | Combine Picture Picture
  | ApplyTransform TX.Transform Picture
  | FillPoly Path

instance Semigroup Picture where
  append p1 p2 = lazy $ Combine p1 p2

instance Monoid Picture where
  mempty = lazy Blank

fill :: Path -> Picture
fill = FillPoly >>> lazy

scale :: Number -> Number -> Picture -> Picture
scale sx sy = ApplyTransform (TX.scale sx sy) >>> lazy

translate :: Number -> Number -> Picture -> Picture
translate tx ty = ApplyTransform (TX.translate tx ty) >>> lazy

rotate :: Number -> Picture -> Picture
rotate a = ApplyTransform (TX.rotate a) >>> lazy

screenSpaceBounds :: Number -> Number -> { right :: Number, bottom :: Number }
screenSpaceBounds w h =
  { right: max 1.0 (w / h)
  , bottom: max 1.0 (h / w)
  }

screenSpace :: Number -> Number -> Picture -> Picture
screenSpace pixelWidth pixelHeight =
  translate (pixelWidth / 2.0) (pixelHeight / 2.0)
    <<< scale s s
  where
  s = min pixelWidth pixelHeight / 2.0

type VisitState =
  { tx :: TX.Transform
  }

data Action
  = SetTransform TX.Transform
  | Fill Path

visitPicture
  :: (Action -> Effect Unit)
  -> TX.Transform
  -> Picture
  -> Effect Unit
visitPicture action initialTx rootPicture = do
  action $ SetTransform initialTx
  evalStateT (go rootPicture) { tx: initialTx }
  where
  go :: Picture -> StateT VisitState Effect Unit
  go (Picture p) = case p unit of
    Blank -> pure unit
    Combine p1 p2 -> do
      { tx: txBefore } <- get
      go p1
      { tx: txAfter } <- get
      unless (unsafeRefEq txBefore txAfter) $ resetTx txBefore
      go p2
    ApplyTransform tx' p' -> do
      tx <- gets (_.tx)
      resetTx (tx' * tx)
      go p'
    FillPoly path -> lift $ action $ Fill path

  resetTx :: TX.Transform -> StateT VisitState Effect Unit
  resetTx tx = do
    modify_ $ \{} -> { tx }
    lift $ action $ SetTransform tx
