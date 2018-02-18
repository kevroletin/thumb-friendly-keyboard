module Parts.Switch (
  Switch(..)
  , switchVertexes
  , switch
  , buildSwitchHole
  , buildKeycapPadHole
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
    w  = switchSocketWidth / 2
    h  = switchHeight / 2

buildSwitchHole :: Switch -> ScadProgram
buildSwitchHole (Switch ts) = transformBySeq ts $ block [
  cube' holeWidth holeWidth (switchHeight + 2) (-1)
  , cube' holeWidth (holeWidth + 2 * holeNotchWidth) switchHeight (-holeNotchHeight)
  ]
  where
    -- cube switchSocketWidth switchSocketWidth switchHeight will exactly match occupy
    -- switch space
    cube' len_x len_y len_z dz =
      translate (-len_x / 2) (-len_y / 2) (-switchHeight / 2 + dz) $
      cube len_x len_y len_z

{-| This hole ensures that switch has clear space for keycap when key is pressed.
  (+ 0.0001) is a trick to preserve socket top surface in OpenScad preview
-}
buildKeycapPadHole :: Switch -> ScadProgram
buildKeycapPadHole (Switch ts) = transformBySeq ts $
  translate 0 0 ((switchHeight + h)/2 + 0.0001) (cube0 switchSocketWidth switchSocketWidth h)
  where h = 4

switch position angles =
  Switch [Translate position, Rotate angles]
