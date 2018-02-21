module Keyboard.Parts.Keycaps (
  buildKeycaps
) where

import qualified Data.Glome.Vec          as V
import           Keyboard.Config
import           Keyboard.Parts.Plate    (Plate)
import           Keyboard.Parts.Switch
import           Keyboard.Scad
import           Keyboard.Scad.Builders
import           Keyboard.Transformation

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

buildKeycaps :: Plate -> ScadProgram
buildKeycaps switches = union (map buildKeycap (concat switches))
