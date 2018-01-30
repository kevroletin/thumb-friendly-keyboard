module Parts.Switch (
  Switch(..)
  , switchVertexes
  , switch
) where

import qualified Data.Glome.Vec as V
import Config
import Transformation

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

switch position angles =
  Switch [Translate position, Rotate angles]
