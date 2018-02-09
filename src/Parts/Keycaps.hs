module Parts.Keycaps (
  buildKeycaps
) where

import qualified Data.Glome.Vec as V
import Config
import Keyboard
import Scad
import Scad.Builders
import Transformation
import Parts.Switch

buildKeycap :: Switch -> ScadProgram
buildKeycap (Switch ts) =
  transformBySeq ts $
  hull [
      translate (-hbw) (-hbw) keycapElevation $
        cube keycapBottomWidth keycapBottomWidth 1
      , translate (-htw) (-htw) (keycapElevation + keycapHeight) $
          cube keycapTopWidth keycapTopWidth 1
  ]
  where
    hbw = keycapBottomWidth / 2
    htw = keycapTopWidth / 2

buildKeycaps :: [Switch] -> ScadProgram
buildKeycaps switches = union (map buildKeycap switches)
