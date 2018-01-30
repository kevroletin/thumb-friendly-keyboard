module Parts.Switch (
  Switch(..)
  , switchVertexes
  , switch
  , buildSwitchHole
) where

import qualified Data.Glome.Vec as V
import Config
import Transformation
import Scad
import Scad.Builders

data Switch = Switch [Transformation] deriving Show;

instance Transformable Switch where
  transform t (Switch ts) = Switch (t : ts)

switchVertexes :: Switch -> [V.Vec]
switchVertexes (Switch ts) =
  map tr [ V.vec (-w) (-w) h
         , V.vec (-w) w    h
         , V.vec w    w    h
         , V.vec w    (-w) h
         , V.vec (-w) (-w) (-h)
         , V.vec (-w) w    (-h)
         , V.vec w    w    (-h)
         , V.vec w    (-w) (-h)
         ]
  where
    tr = transformBySeq ts
    w  = switchWidth / 2
    h  = switchHeight / 2

buildSwitchHole :: Switch -> ScadProgram
buildSwitchHole (Switch ts) = transformBySeq ts $ block [
  cube' holeWidth holeWidth (switchHeight + 2) (-1)
  , cube' holeWidth (holeWidth + 2 * holeNotchWidth) switchHeight (-holeNotchHeight)
  ]
  where
    -- cube switchWidth switchWidth switchHeight will exactly match occupy
    -- switch space
    cube' len_x len_y len_z dz =
      translate (-len_x / 2) (-len_y / 2) (-switchHeight / 2 + dz) $
      cube len_x len_y len_z

switch position angles =
  Switch [Translate position, Rotate angles]
