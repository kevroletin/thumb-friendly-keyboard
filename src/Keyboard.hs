{-| Data types which represents a keyboards.
-}
module Keyboard (
  Switch(..)
  , Envelope(..)
  , switchVertexes
) where

-- TODO: should we re-export?
import qualified Data.Glome.Vec as V
import Config

data Switch = Switch {
  pos :: V.Vec,
  angles :: V.Vec
  } deriving Show;

data Envelope = Envelope {
  left  :: [V.Vec],
  right :: [V.Vec],
  front :: [V.Vec],
  back  :: [V.Vec]
  } deriving Show;

-- TODO: move somewhere else, it's not a keyboard representation
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
