{-|-- We assume next view location:
   -- + x goes from left to right
   -- + y goes from "near" to "far"
-}
module Keyboard.Scad (
  module Keyboard.Scad.Builders
  , Path
  , ScadProgram
  , Segment
  , V.vec
  , V.Vec(..)
  , renderToScad
  , Sandwidge(..)
  , Wall(..)
) where

import qualified Data.Glome.Vec          as V
import           Keyboard.Scad.Builders
import           Keyboard.Scad.Internal
import           Keyboard.Scad.Path
import           Keyboard.Scad.Sandwidge
