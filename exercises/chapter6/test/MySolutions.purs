module Test.MySolutions where

import Prelude

-- Note to reader: Add your solutions to this file
newtype Point
    = Point
    { x :: Number
    , y :: Number
    }

instance showPoint :: Show Point where
    show (Point p) = "Point x:" <> show p.x <> " y:" <> show p.y