module TypeClasses where

import Prelude

newtype Point = Point { x :: Number, y :: Number }

getX :: Point -> Number
getX ( Point p ) = p.x

instance pointShow :: Show Point where
  show (Point p) = "x = " <> (show p.x) <> ", y = " <> (show p.y)