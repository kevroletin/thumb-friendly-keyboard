module Parts.Keycaps (
  buildKeycaps
) where

import qualified Data.Glome.Vec as V
import Keyboard
import Scad
import Config

buildKeycap :: Switch -> ScadProgram
buildKeycap (Switch (V.Vec x y z) (V.Vec ax ay az)) =
  translate x y z $
  rotate ax ay az $
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
