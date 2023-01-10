module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Data.Number (pi, pow, sqrt)

diagonal ∷ Number → Number → Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea n = pi * (pow n 2.0)

leftoverCents :: Int -> Int
leftoverCents n = rem n 100