module Test.MySolutions where

import Data.Maybe (Maybe(..))
import Data.Number (pi, pow)
import Data.Semiring ((*))

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _ = a

data Shape = 
    Circle Point Number
    | Rectangle Point Number Number
    | Line Point Point
    | Text Point String

type Point = {
    x :: Number,
    y :: Number
}

circleAtOrigin :: Shape
circleAtOrigin = Circle {x:0.0, y:0.0} 4.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ n) = Circle {x:0.0,y:0.0} (n*2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle {x:0.0,y:0.0} (w*2.0) (h*2.0)
doubleScaleAndCenter (Line x' y') = Line {x:x'.x*2.0, y:x'.y*2.0} {x:y'.x*2.0, y:y'.y*2.0}
doubleScaleAndCenter (Text _ s) = Text {x:0.0, y:0.0} s

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

area :: Shape -> Number
area (Rectangle _ w h) = w * h
area (Circle _ r) = pi * r * r
area _ = 0.0