module Keyboard.Parts.Keycaps (
  keycaps
  , buildKeycaps
) where

import qualified Data.Glome.Vec             as V
import           Keyboard.Config
import           Keyboard.Parts.Plate       (Plate)
import           Keyboard.Parts.Switch
import           Keyboard.Scad
import           Keyboard.Scad.Builders
import           Keyboard.Scad.HollowFigure
import           Keyboard.Transformation

keycap :: Switch -> ScadProgram
keycap (Switch ts) =
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

keycaps :: Plate -> ScadProgram
keycaps switches = union (map keycap (concat switches))

buildKeycaps :: Plate -> HollowFigure
buildKeycaps plate = HollowFigure (Just $ keycaps plate) Nothing
