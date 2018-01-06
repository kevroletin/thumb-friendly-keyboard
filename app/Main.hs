module Main where

import Lib
import System.IO
import Data.Glome.Vec
import Data.List
import Control.Monad.State

test = xfm_point (rotate (vec 1 0 0) (deg 90)) (vec 0 1 0)

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

switch_width = 10
switch_height = 5
hole_width = 8
hole_height = 12

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

-- we assume next view location:
-- + x goes from left to right
-- + y goes from "near" to "far"
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

render_holes holes = concat (map render_cube holes)

render_difference xs ys =
  "difference() {" : xs ++ ys ++ ["};"]

solve :: [String]
solve = let (connections, (_, verts, holes)) = runState body (0, [], [])
        in render_difference
             (render_polyhedron verts connections)
             (render_holes holes)
  where
    body = do s0 <- switch (vec 0  0  0    ) (vec 0 0 0)
              s1 <- switch (vec 0  20 0    ) (vec 10 0 0)
              s2 <- switch (vec 20 20 (-10)) (vec 10 0 0)
              s3 <- switch (vec 20 0  (-10)) (vec (-10) 0 0)
              return $ concat [
                switch_top_bottom s0
                , switch_top_bottom s1
                , switch_top_bottom s2
                , switch_top_bottom s3
                , connect_left_to_right s1 s2
                , connect_left_to_right s0 s3
                , connect_near_to_far   s0 s1
                , connect_near_to_far   s3 s2
                , connect_middle s0 s1 s2 s3
                , right_wall s2
                , right_wall s3
                , left_wall s0
                , left_wall s1
                , near_wall s0
                , near_wall s3
                , far_wall s1
                , far_wall s2
                , connect_right_wall_near_far s3 s2
                , connect_left_wall_near_far s0 s1
                , connect_near_wall_left_right s0 s3
                , connect_far_wall_left_right s1 s2
                ]

main :: IO ()
main = withFile "result.scad" WriteMode $ \h ->
          mapM_ (hPutStrLn h) solve
