{-|Numerical parameters which affect rendered shapes.
-}
module Config (
  switchWidth
  , switchHeight
  , holeWidth
  , holeNotchWidth
  , holeNotchHeight
  , keycapBottomWidth , keycapTopWidth
  , keycapHeight
  , keycapElevation
  , envelopHeight
) where

switchWidth :: Double
switchWidth = 19.05

switchHeight :: Double
switchHeight = 5

holeWidth :: Double
holeWidth = 13.94

holeNotchWidth :: Double
holeNotchWidth = 0.5

holeNotchHeight :: Double
holeNotchHeight = 1.5

keycapBottomWidth :: Double
keycapBottomWidth = switchWidth

keycapTopWidth :: Double
keycapTopWidth = 14

keycapHeight :: Double
keycapHeight = 10

keycapElevation :: Double
keycapElevation = 6

envelopHeight :: Double
envelopHeight = 5
