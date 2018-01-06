module Main where

import Lib
import System.IO
import Data.Glome.Vec
import Data.List
import Control.Monad.State

test = xfm_point (rotate (vec 1 0 0) (deg 90)) (vec 0 1 0)

data Switch = Switch Vec Vec Vec Vec Vec Vec Vec Vec deriving Show

data Switch' = Switch' Int deriving Show

t0 (Switch' idx) = idx
t1 (Switch' idx) = idx + 1
t2 (Switch' idx) = idx + 2
t3 (Switch' idx) = idx + 3
b0 (Switch' idx) = idx + 4
b1 (Switch' idx) = idx + 5
b2 (Switch' idx) = idx + 6
b3 (Switch' idx) = idx + 7
vert (Switch' idx) n = idx + n
verts sw xs = map (vert sw) xs

switch' :: Vec -> Vec -> State (Int, [Vec]) Switch'
switch' centerPos (Vec ax ay az) = do (lastIdx, vertexes) <- get
                                      put (lastIdx + 8, vertexes ++ [t0, t1, t2, t3, b0, b1, b2, b3])
                                      return (Switch' lastIdx)
  where
    rotation = compose [rotate (vec 1 0 0) (deg ax)
                       , rotate (vec 0 1 0) (deg ay)
                       , rotate (vec 0 0 1) (deg az)]
    move = xfm_point (xfm_mult rotation (translate centerPos))
    w   = 10
    hw  = w/2
    wi  = 8
    hwi = wi/2
    h   = 5
    hh  = h/2
    t0 = move $ vec (-hw) (-hw) hh
    t1 = move $ vec (-hw) hw    hh
    t2 = move $ vec hw    hw    hh
    t3 = move $ vec hw    (-hw) hh
    b0 = move $ vec (-hw) (-hw) (-hh)
    b1 = move $ vec (-hw) hw    (-hh)
    b2 = move $ vec hw    hw    (-hh)
    b3 = move $ vec hw    (-hw) (-hh)

switch centerPos ax ay az = (Switch
     (move $ vec (-hw) (-hw) hh)
     (move $ vec (-hw) hw    hh)
     (move $ vec hw    hw    hh)
     (move $ vec hw    (-hw) hh)
     (move $ vec (-hw) (-hw) (-hh))
     (move $ vec (-hw) hw    (-hh))
     (move $ vec hw    hw    (-hh))
     (move $ vec hw    (-hw) (-hh)))
  where
    rotation = compose [rotate (vec 1 0 0) (deg ax)
                       , rotate (vec 0 1 0) (deg ay)
                       , rotate (vec 0 0 1) (deg az)]
    move = xfm_point (xfm_mult rotation (translate centerPos))
    w   = 10
    hw  = w/2
    wi  = 8
    hwi = wi/2
    h   = 5
    hh  = h/2

vec_to_scad (Vec x y z) = "[" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ "]"

switch_to_scad sw = sandwich $ verts sw [0, 1, 2, 3]

connect_by_square t0 t1 t2 t3 = [
  "polyhedron("
  , "points=["
  , intercalate ",\n" (fmap vec_to_scad [t0, t1, t2, t3])
  , "],"
  , "faces=["
  ,   "[0, 1, 2, 3]," , "]);"
  ]

top_to_bottom = (+ 4)
sandwich xs = [xs, reverse $ map top_to_bottom xs]

-- we assume next view location:
-- + x goes from left to right
-- + y goes from "near" to "far"
connect_left_to_right l r = sandwich [t1 r, t0 r, t3 l, t2 l]

connect_near_to_far n f = sandwich [t1 n, t0 f, t3 f, t2 n]

connect_middle s0 s1 s2 s3 = sandwich [t2 s0, t3 s1, t0 s2, t1 s3]

right_wall (Switch t0 t1 t2 t3 b0 b1 b2 b3) =
  (connect_by_square t3 t2 b2 b3)

left_wall (Switch t0 t1 t2 t3 b0 b1 b2 b3) =
  (connect_by_square t1 t0 b0 b1)

near_wall (Switch t0 t1 t2 t3 b0 b1 b2 b3) =
  (connect_by_square t0 t3 b3 b0)

far_wall (Switch t0 t1 t2 t3 b0 b1 b2 b3) =
  (connect_by_square t1 t2 b2 b1)

connect_right_wall_near_far (Switch t0 t1 t2 t3 b0 b1 b2 b3) (Switch t0' t1' t2' t3' b0' b1' b2' b3') =
  (connect_by_square t2 t3' b3' b2)

connect_left_wall_near_far (Switch t0 t1 t2 t3 b0 b1 b2 b3) (Switch t0' t1' t2' t3' b0' b1' b2' b3') =
  (connect_by_square t0' t1 b1 b0')

connect_near_wall_left_right (Switch t0 t1 t2 t3 b0 b1 b2 b3) (Switch t0' t1' t2' t3' b0' b1' b2' b3') =
  (connect_by_square t3 t0' b0' b3)

connect_far_wall_left_right (Switch t0 t1 t2 t3 b0 b1 b2 b3) (Switch t0' t1' t2' t3' b0' b1' b2' b3') =
  (connect_by_square t1' t2 b2 b1')

solve :: [String]
solve = let (connections, (_, verts)) = runState body (0, [])
        in ["polyhedron("
           , "points=["
           , intercalate ",\n" (fmap vec_to_scad verts)
           , "],"
           , "faces=["
           , intercalate ",\n" (fmap show connections)
           , "]);"
           ]
  where
    body = do s0 <- switch' (vec 0  0  0    ) (vec 0 0 0)
              s1 <- switch' (vec 0  20 0    ) (vec 10 0 0)
              s2 <- switch' (vec 20 20 (-10)) (vec 10 0 0)
              s3 <- switch' (vec 20 0  (-10)) (vec (-10) 0 0)
              return $ concat [
                switch_to_scad s0
                , switch_to_scad s1
                , switch_to_scad s2
                , switch_to_scad s3
                , connect_left_to_right s1 s2
                , connect_left_to_right s0 s3
                , connect_near_to_far   s0 s1
                , connect_near_to_far   s3 s2
                , connect_middle s0 s1 s2 s3
                ]
-- solve' :: [String]
-- solve' = concat [
--   , right_wall s2
--   , right_wall s3
--   , left_wall s0
--   , left_wall s1
--   , near_wall s0
--   , near_wall s3
--   , far_wall s1
--   , far_wall s2
--   , connect_right_wall_near_far s3 s2
--   , connect_left_wall_near_far s0 s1
--   , connect_near_wall_left_right s0 s3
--   , connect_far_wall_left_right s1 s2
--   ]
--   where

main :: IO ()
main = withFile "result.scad" WriteMode $ \h ->
          mapM_ (hPutStrLn h) solve
