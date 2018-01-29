module Parts.Common (
  switchVertexes
) where

import qualified Data.Glome.Vec as V
import Keyboard
import Config

switchVertexes :: Switch -> [V.Vec]
switchVertexes (Switch centerPos (V.Vec ax ay az)) =
  [ move $ V.vec (-w) (-w) h
  , move $ V.vec (-w) w    h
  , move $ V.vec w    w    h
  , move $ V.vec w    (-w) h
  , move $ V.vec (-w) (-w) (-h)
  , move $ V.vec (-w) w    (-h)
  , move $ V.vec w    w    (-h)
  , move $ V.vec w    (-w) (-h)
  ]
  where
    rotation = V.compose [V.rotate (V.vec 1 0 0) (V.deg ax)
                         , V.rotate (V.vec 0 1 0) (V.deg ay)
                         , V.rotate (V.vec 0 0 1) (V.deg az)]
    move = V.xfm_point (V.xfm_mult (V.translate centerPos) rotation)
    w  = switchWidth / 2
    h  = switchHeight / 2
