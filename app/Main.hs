module Main where

import Lib
import System.IO
import Data.Glome.Vec
import Data.List
import Control.Monad.State

test = xfm_point (rotate (vec 1 0 0) (deg 90)) (vec 0 1 0)

-- we assume next view location:
-- + x goes from left to right
-- + y goes from "near" to "far"
data Switch = Switch Int deriving Show

t0 (Switch idx) = idx
t1 (Switch idx) = idx + 1
t2 (Switch idx) = idx + 2
t3 (Switch idx) = idx + 3
b0 (Switch idx) = idx + 4
b1 (Switch idx) = idx + 5
b2 (Switch idx) = idx + 6
b3 (Switch idx) = idx + 7
vert (Switch idx) n = idx + n
verts sw xs = map (vert sw) xs

switch_width = 19.05
switch_height = 5
hole_width = 14
hole_height = 12
keycap_bottom_width = 19.05
keycap_top_width = 14
keycap_height = 10
keycap_elevation = 6

-- Main idea here is to defined set of vertexes for polyhedron and then
-- reference vertexes using indexes. All data about socket is stored in state
-- and socket contains indexes which point to state.
switch :: Vec -> Vec -> State (Int, [Vec], [(Vec, Vec)]) Switch
switch centerPos (Vec ax ay az) = do
  (lastIdx, vertexes, holes) <- get
  put (lastIdx + 8
      , vertexes ++ [t0, t1, t2, t3, b0, b1, b2, b3]
      , holes ++ [hole])
  return (Switch lastIdx)
  where
    rotation = compose [rotate (vec 1 0 0) (deg ax)
                       , rotate (vec 0 1 0) (deg ay)
                       , rotate (vec 0 0 1) (deg az)]
    move = xfm_point (xfm_mult (translate centerPos) rotation)
    hw  = switch_width/2
    hwi = hole_width/2
    hh  = switch_height/2
    t0 = move $ vec (-hw) (-hw) hh
    t1 = move $ vec (-hw) hw    hh
    t2 = move $ vec hw    hw    hh
    t3 = move $ vec hw    (-hw) hh
    b0 = move $ vec (-hw) (-hw) (-hh)
    b1 = move $ vec (-hw) hw    (-hh)
    b2 = move $ vec hw    hw    (-hh)
    b3 = move $ vec hw    (-hw) (-hh)
    hole = (centerPos, Vec ax ay az)

render_vec (Vec x y z) = "[" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ "]"

switch_top_bottom sw = sandwich $ verts sw [0, 1, 2, 3]

top_to_bottom = (+ 4)
sandwich xs = [xs, reverse $ map top_to_bottom xs]

connect_left_to_right l r = sandwich [t1 r, t0 r, t3 l, t2 l]

connect_near_to_far n f = sandwich [t1 n, t0 f, t3 f, t2 n]

connect_middle s0 s1 s2 s3 = sandwich [t2 s0, t3 s1, t0 s2, t1 s3]

right_wall sw = [verts sw [3, 2, 6, 7]]

left_wall sw = [verts sw [4, 5, 1, 0]]

near_wall sw = [verts sw [0, 3, 7, 4]]

far_wall sw = [verts sw [2, 1, 5, 6]]

connect_right_wall_near_far n f = [[t2 n, t3 f, b3 f, b2 n]]

connect_left_wall_near_far n f = [[t0 f, t1 n, b1 n, b0 f]]

connect_near_wall_left_right l r = [[t3 l, t0 r, b0 r, b3 l]]

connect_far_wall_left_right l r = [[t1 r, t2 l, b2 l, b1 r]]

render_polyhedron verts connections = [
  "polyhedron("
  , "points=["
  , intercalate ",\n" (fmap render_vec verts)
  , "],"
  , "faces=["
  , intercalate ",\n" (fmap show connections)
  , "]);"
  ]

render_cube (Vec x y z, Vec ax ay az) = [
  "translate(" ++ show [x, y, z] ++ ")"
  , "rotate(" ++ show [ax, ay, az] ++ ")"
  , "translate(" ++ show [-hw, -hw, -hh] ++ ")"
  , "cube(" ++ show [hole_width, hole_width, hole_height] ++ ");"
  ]
  where
    hw = hole_width / 2
    hh = hole_height / 2

render_keycap (Vec x y z, Vec ax ay az) = [
  "translate(" ++ show [x, y, z] ++ ")"
  , "rotate(" ++ show [ax, ay, az] ++ ")"
  , "hull() {"
  , "  translate(" ++ show [-hbw, -hbw, keycap_elevation] ++ ")"
  , "    cube(" ++ show [keycap_bottom_width, keycap_bottom_width, 1] ++ ");"
  , "  translate(" ++ show [-htw, -htw, (keycap_elevation + keycap_height)] ++ ")"
  , "    cube(" ++ show [keycap_top_width, keycap_top_width, 1] ++ ");"
  , "};"
  ]
  where
    hbw = keycap_bottom_width / 2
    htw = keycap_top_width / 2

render_holes holes = concat (map render_cube holes)

render_keycaps holes = concat (map render_keycap holes)

render_difference xs ys =
  "difference() {" : xs ++ ys ++ ["};"]

render_union xs ys =
  "union() {" : xs ++ ys ++ ["};"]

color c xs = "color(" : [show c] ++ [") {"] ++ xs ++ ["}"]

render coords withKeycaps = let (connections, (_, verts, holes)) = runState body (0, [], [])
        in render_difference
             (if not withKeycaps then render_polyhedron verts connections
              else render_union
                     (color [1, 1, 1] $ render_keycaps holes)
                     (color [0.8, 0.8, 0.8] $ render_polyhedron verts connections))
             (render_holes holes)
  where
    width = length (head coords)
    height = length coords
    body = do sw <- sequence (fmap sequence coords)
              let top_bottom = concat . concat $ map (map switch_top_bottom) sw
              let near_far = concat [connect_near_to_far (sw !! y !! x) (sw !! (y + 1) !! x) |
                                      y <- [0 .. (height - 2)],
                                      x <- [0 .. (width - 1)]]
              let left_right = concat [connect_left_to_right (sw !! y !! x) (sw !! y !! (x + 1)) |
                                        y <- [0 .. (height - 1)],
                                        x <- [0 .. (width - 2)]]
              let middle = concat [connect_middle (sw !! y !! x) (sw !! (y + 1) !! x) (sw !! (y + 1) !! (x + 1)) (sw !! y !! (x + 1)) |
                                    y <- [0 .. (height - 2)],
                                    x <- [0 .. (width - 2)]]
              let right = concat [right_wall (sw !! y            !! (width - 1))  | y <- [0 .. (height - 1)]]
              let left  = concat [left_wall  (sw !! y            !! 0)            | y <- [0 .. (height - 1)]]
              let far   = concat [far_wall   (sw !! (height - 1) !! x)            | x <- [0 .. (width - 1)]]
              let near  = concat [near_wall  (sw !! 0            !! x)            | x <- [0 .. (width - 1)]]
              let right' = concat [connect_right_wall_near_far (sw !! y !! (width - 1)) (sw !! (y + 1) !! (width - 1)) |
                                    y <- [0 .. (height - 2)]]
              let left'  = concat [connect_left_wall_near_far (sw !! y !! 0) (sw !! (y + 1) !! 0) |
                                    y <- [0 .. (height - 2)]]
              let near'  = concat [connect_near_wall_left_right (sw !! 0 !! x) (sw !! 0 !! (x + 1)) |
                                    x <- [0 .. (width - 2)]]
              let far'  = concat [connect_far_wall_left_right (sw !! (height - 1) !! x) (sw !! (height - 1) !! (x + 1)) |
                                   x <- [0 .. (width - 2)]]
              return $ top_bottom ++ near_far ++ left_right ++ middle ++ right ++ right' ++ left ++ left' ++ far ++ far' ++ near ++ near'

mainPlate = [
  [ switch (vec 0  (0) 7) (vec (-25) 10 5)
  , switch (vec 22 (2) 4) (vec (-25) 0 5)
  , switch (vec 44 (2) 4) (vec (-25) 0 0)
  , switch (vec 66 (4) 4) (vec (-25) 0 0)
  , switch (vec 90 (-2) 6) (vec (-25) (-10) (-15))
  , switch (vec 111 (-6) 12) (vec (-25) (-20) (-15))
  ],
  [ switch (vec 0  23 2) (vec (0) 10 5)
  , switch (vec 22 25 0) (vec (0) 0 5)
  , switch (vec 44 28 0) (vec (0) 0 0)
  , switch (vec 66 30 0) (vec (0) 0 0)
  , switch (vec 90 22 2) (vec (0) (-10) (-15))
  , switch (vec 111 18 09) (vec (0) (-20) (-15))
  ],
  [ switch (vec 0  46 8) (vec (25) 10 5)
  , switch (vec 22 48 6) (vec (25) 0 5)
  , switch (vec 44 54 6) (vec (25) 0 0)
  , switch (vec 66 56 6) (vec (25) 0 0)
  , switch (vec 90 46 8) (vec (25) (-10) (-15))
  , switch (vec 111 42 15) (vec (30) (-23) (-15))
  ]
  ]

thumbPlate = [
  [ switch (vec 0  0 6) (vec (-15) (15) 0)
  , switch (vec 22 0 3) (vec (-15) 0 0)
  , switch (vec 44 0 6) (vec (-15) (-15) 0)
  ],
  [ switch (vec 0  22 3) (vec 0 (15) 0)
  , switch (vec 22 22 0) (vec 0 0 0)
  , switch (vec 44 22 3) (vec 0 (-15) 0)
  ],
  [ switch (vec 0  44 6) (vec (15) (15) 0)
  , switch (vec 22 44 3) (vec (15) 0 0)
  , switch (vec 44 44 6) (vec (15) (-15) 0)
  ]
  ]

withKeycaps = False

main :: IO ()
main = do withFile "main_plate.scad" WriteMode $ \h -> mapM_ (hPutStrLn h) (render mainPlate withKeycaps)
          withFile "thumb_pad.scad" WriteMode $ \h -> mapM_ (hPutStrLn h) (render thumbPlate withKeycaps)
