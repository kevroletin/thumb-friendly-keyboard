-- | Purpose of there modules is to fill space between two paths. Proper
-- approach would be to user spline (such operation is often called loft).
-- Current implementation contains only quick hacks. OpenScad doesn't have
-- builtin splines and 3rd party OpenScad splines aren't easy to use and I am
-- not interested enough to write my own. So we just split paths into equal
-- number of small segments and connect corresponding segments by triangles or
-- or figures.

module Keyboard.Scad.Connectors () where
