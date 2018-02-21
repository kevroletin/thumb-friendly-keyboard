module Demo.Body (
  bodyDemo
) where

import           Data.Monoid
import           Keyboard.Config
import           Keyboard.GeneralUtils
import           Keyboard.Parts
import           Keyboard.Scad
import           Keyboard.Scad
import qualified Keyboard.Scad.Connectors.Sphere as Sp
import           Keyboard.Scad.HollowFigure
import           Keyboard.Scad.Sandwidge
import           Keyboard.Transformation

bodyDemo mainPlate thumbPlate = union [
  plateSolid p1
  , plateSolid p2
  , keycaps p1
  , keycaps p2

  , Sp.connectPaths 2.5 (wallMiddle $ plateFrontWall p1)
    (wallMiddle $ reverseWall $ plateBackWall p2 <> plateLeftWall p2)
  , Sp.projectWallDown (plateLeftWall p1)
  , Sp.projectWallDown (plateBackWall p1)
  , Sp.projectWallDown (plateRightWall p2)
  , Sp.projectWallDown (plateFrontWall p2)
  , Sp.projectPathDown 2.5 [
      (head $ wallMiddle (plateFrontWall p1))
      , (last $ wallMiddle (plateLeftWall p2))
      ]
  ]
  where
    tr1 = transformBySeq [
      Translate (vec 0 0 10)
      , Rotate (vec 0 (-60) 0)
      ]
    tr2 = transformBySeq [
      Translate (vec 45 (-45) 25)
      , Rotate (vec 50 (-20) 0)
      ]
    p1 = (tr1 mainPlate)
    p2 = (tr2 thumbPlate)
