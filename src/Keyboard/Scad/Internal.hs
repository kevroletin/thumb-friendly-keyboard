module Keyboard.Scad.Internal (
  ScadProgram(..)
  , PolyhedronSurface
  , OperatorParams(..)
  , isBlcok
  , render
  , renderToScad
) where

import qualified Data.Glome.Vec          as V
import qualified Data.List               as List
import           Keyboard.Transformation

type PolyhedronSurface = [Int]

data OperatorParams = NoParams
                    | VecParams V.Vec
                    | StringParams String
                    deriving Show

-- + we can create transformation of transformation which doesn't make sense;
-- + Monoid to compose figures using <> instead of union;
-- + implement comments to make resulting scad program debuggable.
data ScadProgram =
  Polyhedron { polyhedronPoints :: [V.Vec]
             , polyhedronFaces  :: [PolyhedronSurface]}
  | Cube V.Vec
  | Block [ScadProgram]
  | Operator String OperatorParams ScadProgram
  | Sphere { sphereRadius :: Double }
  deriving Show

isBlcok :: ScadProgram -> Bool
isBlcok (Block _) = True
isBlcok _         = False

renderVec :: V.Vec -> String
renderVec (V.Vec x y z) = show [x, y, z]

renderSurface :: PolyhedronSurface -> String
renderSurface xs = show xs

render :: ScadProgram -> [String]
render (Polyhedron verts faces) =
  [
    "polyhedron("
  , "points=["
  , List.intercalate ",\n" (map renderVec verts)
  , "],"
  , "faces=["
  , List.intercalate ",\n" (map renderSurface faces)
  , "]);"
  ]
render (Cube dim) = ["cube(" ++ renderVec dim ++ ");"]
render (Operator name params child) =
  if isBlcok child
    then (title ++ " {") : render child ++ ["};"]
    else title : render child
  where paramsStr NoParams      = ""
        paramsStr (VecParams v) = renderVec v
        title = (name ++ "(" ++ (paramsStr params) ++ ")")
render (Block childs) = concatMap render childs
render (Sphere r) = ["sphere(" ++ show r ++ ");"]

renderToScad :: ScadProgram -> [Char]
renderToScad = (List.intercalate "\n") . render
