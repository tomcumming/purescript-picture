module Picture.Transform
  ( Transform
  , TxObject
  , asObject
  , scale
  , translate
  , rotate
  ) where

import Prelude

import Data.Number (cos, sin)

type TxObject =
  { m11 :: Number
  , m12 :: Number
  , m21 :: Number
  , m22 :: Number
  , m41 :: Number
  , m42 :: Number
  }

newtype Transform = Transform TxObject

instance Semiring Transform where
  zero = Transform { m11: 0.0, m12: 0.0, m21: 0.0, m22: 0.0, m41: 0.0, m42: 0.0 }
  one = Transform { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m41: 0.0, m42: 0.0 }
  add (Transform t1) (Transform t2) = Transform
    { m11: t1.m11 + t2.m11
    , m12: t1.m12 + t2.m12
    , m21: t1.m21 + t2.m21
    , m22: t1.m22 + t2.m22
    , m41: t1.m41 + t2.m41
    , m42: t1.m42 + t2.m42
    }
  mul (Transform t1) (Transform t2) = Transform
    { m11: t1.m11 * t2.m11 + t1.m12 * t2.m21
    , m12: t1.m11 * t2.m12 + t1.m12 * t2.m22
    , m21: t1.m21 * t2.m11 + t1.m22 * t2.m21
    , m22: t1.m21 * t2.m12 + t1.m22 * t2.m22
    , m41: t1.m41 * t2.m11 + t1.m42 * t2.m21 + t2.m41
    , m42: t1.m41 * t2.m12 + t1.m42 * t2.m22 + t2.m42
    }

asObject :: Transform -> TxObject
asObject (Transform tx) = tx

scale :: Number -> Number -> Transform
scale m11 m22 = Transform { m11, m12: 0.0, m21: 0.0, m22, m41: 0.0, m42: 0.0 }

translate :: Number -> Number -> Transform
translate m41 m42 = Transform { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m41, m42 }

rotate :: Number -> Transform
rotate a = Transform
  { m11: cos a
  , m12: sin a
  , m21: negate (sin a)
  , m22: cos a
  , m41: 0.0
  , m42: 0.0
  }
