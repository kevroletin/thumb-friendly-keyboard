-- | = Plate generator for 3d split keyboard.
--
-- A __plate__ is a surface with sockets for key switches. This generator
-- produces plates like this /(for Cherry MX switches)/:
--
-- <<doc/img/main_plate_half.png Plate>>
--
-- __\"3d\"__ means that a plate is a curved figure (as opposed to a "flat"
-- plate made from a metal sheet). The generator produces a plate using manually
-- created config of sockets positions and angles.
--
-- __Split__ keyboard means that keyboard consists of two pars for left and
-- right hand. Although currently, it makes no difference because this module
-- generates rectangle plates which can be used both for the split and ordinary
-- keyboards.
--
-- == Why
--
-- This generator produces Open Scad programs. Open Scad is a programming
-- language itself but it has one big limitation. Open Scad provides no way to
-- calculate point location after series of transformations. This is a stopper
-- for such project because coordinates are required to connect manually placed
-- sockets.
--
-- There are two main objects in this module:
--
--     * "Parts" - keyboard parts like a plate, keycaps, etc.
--
--     * "Scad" - representation of OpenScad programs. It contains base
--     * primitives from OpenScad language such as a cube, sphere, hull, etc.
--     * and more complicated composite figures as SphereLine and others.
--
-- General idea is that Parts are composed and transformed into Scad. Please
-- note that both Parts and Scad objects can be transformed (translated or
-- rotated). However, usually, it is better to transform Parts because it is
-- possible to get coordinates out of transformed Part.
--
-- == Conventions
--
-- Implementation of this module uses several naming conventions.
--
--     * build* functions produce HollowFigures /(which are figures together
--     * with parts what should be removed)/
--
--     * we assume next view location /(this is default view location in
--     * OpenScad)/:
--
--         * x goes from left to right
--         * y goes from "front" to "back"

module Keyboard (
  module Keyboard.Parts
  , module Keyboard.Scad
) where

import Keyboard.Parts
import Keyboard.Scad
import Keyboard.Config
