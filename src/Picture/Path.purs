module Picture.Path where

import Prelude

import Data.Array (head, snoc)
import Data.Maybe (Maybe(..))

type Path = Array { x :: Number, y :: Number }

close :: Path -> Path
close path = case head path of
  Nothing -> path
  Just pt -> snoc path pt

absoluteRect :: Number -> Number -> Number -> Number -> Path
absoluteRect sx sy w h =
  [ { x: sx, y: sy }
  , { x: sx, y: sy + h }
  , { x: sx + w, y: sy + h }
  , { x: sx + w, y: sy }
  ]

rect :: Path
rect = absoluteRect (-1.0) (-1.0) 2.0 2.0
