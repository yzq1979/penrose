{-# LANGUAGE ExplicitForAll #-}

module Penrose.Functions.Constraints
  ( constrFuncDict
  , constrSignatures
  ) where

import qualified Data.Map.Strict         as M
import qualified Data.MultiMap           as MM
import           Debug.Trace
import           Penrose.Functions.Types
import           Penrose.Shapes
import           Penrose.Transforms
import           Penrose.Util

--------------------------------------------------------------------------------
-- Constraints
-- exterior point method: penalty function
penalty :: (Ord a, Floating a, Show a) => a -> a
penalty x = max x 0 ^ q -- weights should get progressively larger in cr_dist
  where
    q = 2 :: Int -- also, may need to sample OUTSIDE feasible set
            -- where q = 3

--
{-# INLINE penalty #-}
-- | 'constrFuncDict' stores a mapping from the name of constraint functions to the actual implementation
constrFuncDict ::
     forall a. (Autofloat a)
  => M.Map FuncName (ConstrFnOn a)
constrFuncDict = M.fromList $ map toPenalty flist
  where
    toPenalty (n, f) = (n, penalty . f)
    flist =
      [ ("at", at)
      , ("contains", contains)
    --   , ("sameHeight", sameHeight)
    --   , ("nearHead", nearHead)
      , ("smallerThan", smallerThan)
      , ("minSize", minSize)
      , ("maxSize", maxSize)
      , ("outsideOf", outsideOf)
      , ("overlapping", overlapping)
      , ("disjoint", disjoint)
      , ("inRange", (*) indivConstrWeight . inRange')
      , ("lessThan", lessThan)
      , ("lessThanSq", lessThanSq)
      , ("onCanvas", onCanvas)
      , ("unit", unit')
      , ("equal", equalFn)
      , ("hasNorm", hasNorm)
      , ("hasLorenzNorm", hasLorenzNorm)
      , ("atDist", atDist)
      , ("labelDisjoint", labelDisjointConstr)
      , ("perpendicular", perpendicularConstr)
      ]

indivConstrWeight :: (Autofloat a) => a
indivConstrWeight = 1

constrSignatures :: OptSignatures
constrSignatures =
  MM.fromList
    [ ("at", [AnyGPI, ValueT FloatT, ValueT FloatT])
    , ("minSize", [AnyGPI])
    , ("maxSize", [AnyGPI])
    , ("smallerThan", [GPIType "Circle", GPIType "Circle"])
    , ("smallerThan", [GPIType "Circle", GPIType "Square"])
    , ("smallerThan", [GPIType "Square", GPIType "Circle"])
    , ("smallerThan", [GPIType "Square", GPIType "Square"])
    , ("outsideOf", [GPIType "Text", GPIType "Circle"])
    , ("contains", [GPIType "Circle", GPIType "Circle"])
    , ("contains", [GPIType "Square", GPIType "Arrow"])
    , ("contains", [GPIType "Circle", GPIType "Circle", ValueT FloatT])
    , ("contains", [GPIType "Circle", GPIType "Text"])
    , ("contains", [GPIType "Square", GPIType "Text"])
    , ("contains", [GPIType "Rectangle", GPIType "Text"])
    , ("contains", [GPIType "Square", GPIType "Circle", ValueT FloatT])
    , ("contains", [GPIType "Square", GPIType "Circle"])
    , ("contains", [GPIType "Circle", GPIType "Square"])
    , ("contains", [GPIType "Circle", GPIType "Rectangle"])
    , ("overlapping", [GPIType "Circle", GPIType "Circle"])
    , ("overlapping", [GPIType "Square", GPIType "Circle"])
    , ("overlapping", [GPIType "Circle", GPIType "Square"])
    , ("overlapping", [GPIType "Square", GPIType "Square"])
    , ("disjoint", [GPIType "Circle", GPIType "Circle"])
    , ("disjoint", [GPIType "Square", GPIType "Square"])
        -- ("lessThan", []) --TODO
    ]

--------------------------------------------------------------------------------
-- Constraint Functions
at :: ConstrFn
at [GPI o, Val (FloatV x), Val (FloatV y)] = (getX o - x) ^ 2 + (getY o - y) ^ 2

lessThan :: ConstrFn
lessThan [Val (FloatV x), Val (FloatV y)] = x - y

lessThanSq :: ConstrFn
lessThanSq [Val (FloatV x), Val (FloatV y)] =
  if x < y
    then 0
    else (x - y) ^ 2
           -- in trace ("lessThan, x: " ++ show x ++ ", y: " ++ show y ++ ", res: " ++ show res) res

contains :: ConstrFn
contains [GPI o1@("Circle", _), GPI o2@("Circle", _)] =
  dist (getX o1, getY o1) (getX o2, getY o2) - (getNum o1 "r" - getNum o2 "r")
contains [GPI outc@("Circle", _), GPI inc@("Circle", _), Val (FloatV padding)] =
  dist (getX outc, getY outc) (getX inc, getY inc) -
  (getNum outc "r" - padding - getNum inc "r")
contains [GPI c@("Circle", _), GPI rect@("Rectangle", _)] =
  let (x, y, w, h) =
        (getX rect, getY rect, getNum rect "sizeX", getNum rect "sizeY")
      [x0, x1, y0, y1] = [x - w / 2, x + w / 2, y - h / 2, y + h / 2]
      pts = [(x0, y0), (x0, y1), (x1, y0), (x1, y1)]
      (cx, cy, radius) = (getX c, getY c, getNum c "r")
  in sum $ map (\(a, b) -> max 0 $ dist (cx, cy) (a, b) - radius) pts
contains [GPI c@("Circle", _), GPI t@("Text", _)] =
  let res =
        dist (getX t, getY t) (getX c, getY c) - getNum c "r" +
        max (getNum t "w") (getNum t "h")
  in if res < 0
       then 0
       else res
    -- TODO: factor out the vertex access code to a high-level getter
    -- NOTE: seems that the following version doesn't perform as well as the hackier old version. Maybe it's the shape of the obj that is doing it, but we do observe that the labels tend to get really close to the edges
    -- let (x, y, w, h)     = (getX t, getY t, getNum t "w", getNum t "h")
    --     [x0, x1, y0, y1] = [x - w/2, x + w/2, y - h/2, y + h/2]
    --     pts              = [(x0, y0), (x0, y1), (x1, y0), (x1, y1)]
    --     (cx, cy, radius) = (getX c, getY c, getNum c "r")
    -- in sum $ map (\(a, b) -> (max 0 $ dist (cx, cy) (a, b) - radius)^2) pts
contains [GPI s@("Square", _), GPI l@("Text", _)] =
  dist (getX l, getY l) (getX s, getY s) - getNum s "side" / 2 +
  getNum l "w" / 2
contains [GPI r@("Rectangle", _), GPI l@("Text", _), Val (FloatV padding)]
    -- TODO: implement precisely, max (w, h)? How about diagonal case?
 =
  dist (getX l, getY l) (getX r, getY r) - getNum r "sizeX" / 2 +
  getNum l "w" / 2 +
  padding
contains [GPI r@("Rectangle", _), GPI c@("Circle", _), Val (FloatV padding)]
             -- HACK: reusing test impl, revert later
 =
  let r_l = min (getNum r "sizeX") (getNum r "sizeY") / 2
      diff = r_l - getNum c "r"
  in dist (getX r, getY r) (getX c, getY c) - diff + padding
contains [GPI outc@("Square", _), GPI inc@("Square", _)] =
  dist (getX outc, getY outc) (getX inc, getY inc) -
  (0.5 * getNum outc "side" - 0.5 * getNum inc "side")
contains [GPI outc@("Square", _), GPI inc@("Circle", _)] =
  dist (getX outc, getY outc) (getX inc, getY inc) -
  (0.5 * getNum outc "side" - getNum inc "r")
contains [GPI outc@("Square", _), GPI inc@("Circle", _), Val (FloatV padding)] =
  dist (getX outc, getY outc) (getX inc, getY inc) -
  (0.5 * getNum outc "side" - padding - getNum inc "r")
contains [GPI outc@("Circle", _), GPI inc@("Square", _)] =
  dist (getX outc, getY outc) (getX inc, getY inc) -
  (getNum outc "r" - 0.5 * getNum inc "side")
contains [GPI set@("Ellipse", _), GPI label@("Text", _)] =
  dist (getX label, getY label) (getX set, getY set) -
  max (getNum set "rx") (getNum set "ry") +
  getNum label "w"
contains [GPI e@("Ellipse", _), GPI c@("Circle", _)] =
  dist (getX c, getY c) (getX e, getY e) - max (getNum e "rx") (getNum e "ry") +
  getNum c "r"
contains [GPI e@("Ellipse", _), GPI c@("Circle", _), Val (FloatV padding)] =
  dist (getX c, getY c) (getX e, getY e) - max (getNum e "rx") (getNum e "ry") +
  getNum c "r" +
  padding
-- TODO: combine Line and Arrow cases (the code is the same!!)
contains [GPI sq@("Square", _), GPI ar@("Arrow", _)] =
  let (startX, startY, endX, endY) = arrowPts ar
      (x, y) = (getX sq, getY sq)
      side = getNum sq "side"
      (lx, ly) = ((x - side / 2) * 0.75, (y - side / 2) * 0.75)
      (rx, ry) = ((x + side / 2) * 0.75, (y + side / 2) * 0.75)
  in inRange startX lx rx + inRange startY ly ry + inRange endX lx rx +
     inRange endY ly ry
contains [GPI rt@("Rectangle", _), GPI ar@("Arrow", _)] =
  let (startX, startY, endX, endY) = arrowPts ar
      (x, y) = (getX rt, getY rt)
      (w, h) = (getNum rt "sizeX", getNum rt "sizeY")
      (lx, ly) = (x - w / 2, y - h / 2)
      (rx, ry) = (x + w / 2, y + h / 2)
  in inRange startX lx rx + inRange startY ly ry + inRange endX lx rx +
     inRange endY ly ry
contains [GPI r@("Rectangle", _), Val (TupV (x, y)), Val (FloatV padding)] =
  let r_l = min (getNum r "sizeX") (getNum r "sizeY") / 2
  in dist (getX r, getY r) (x, y) - r_l + padding
contains [GPI sq@("Square", _), GPI ar@("Line", _)] =
  let (startX, startY, endX, endY) = arrowPts ar
      (x, y) = (getX sq, getY sq)
      side = getNum sq "side"
      (lx, ly) = ((x - side / 2) * 0.75, (y - side / 2) * 0.75)
      (rx, ry) = ((x + side / 2) * 0.75, (y + side / 2) * 0.75)
  in inRange startX lx rx + inRange startY ly ry + inRange endX lx rx +
     inRange endY ly ry
contains [GPI rt@("Rectangle", _), GPI ar@("Line", _)] =
  let (startX, startY, endX, endY) = arrowPts ar
      (x, y) = (getX rt, getY rt)
      (w, h) = (getNum rt "sizeX", getNum rt "sizeY")
      (lx, ly) = (x - w / 2, y - h / 2)
      (rx, ry) = (x + w / 2, y + h / 2)
  in inRange startX lx rx + inRange startY ly ry + inRange endX lx rx +
     inRange endY ly ry

inRange a l r
  | a < l = (a - l) ^ 2
  | a > r = (a - r) ^ 2
  | otherwise = 0

inRange'' :: (Autofloat a) => a -> a -> a -> a
inRange'' v left right
  | v < left = left - v
  | v > right = v - right
  | otherwise = 0

inRange' :: ConstrFn
inRange' [Val (FloatV v), Val (FloatV left), Val (FloatV right)]
  | v < left = left - v
  | v > right = v - right
  | otherwise = 0

-- = inRange v left right
onCanvas :: ConstrFn
onCanvas [GPI g] =
  let (leftX, rightX) = (-canvasHeight / 2, canvasHeight / 2)
      (leftY, rightY) = (-canvasWidth / 2, canvasWidth / 2)
  in inRange'' (getX g) (r2f leftX) (r2f rightX) +
     inRange'' (getY g) (r2f leftY) (r2f rightY)

unit' :: ConstrFn
unit' [Val (ListV vec)] = hasNorm [Val (ListV vec), Val (FloatV 1)]

-- | This is an equality constraint (x = c) via two inequality constraints (x <= c and x >= c)
equal' :: Autofloat a => a -> a -> a
equal' x y =
  let vals = (x, y)
      (val_max, val_min) = (uncurry max vals, uncurry min vals)
  in val_max - val_min

equalFn :: ConstrFn
equalFn [Val (FloatV x), Val (FloatV y)] = equal' x y

hasNorm :: ConstrFn
hasNorm [Val (ListV vec), Val (FloatV desired_norm)] -- TODO: Use normal norm or normsq?
 = equal' (norm vec) desired_norm

hasLorenzNorm :: ConstrFn
hasLorenzNorm [Val (ListV vec), Val (FloatV desired_norm)] =
  let norms = (normsqLor vec, desired_norm)
      (norm_max, norm_min) = (uncurry max norms, uncurry min norms)
  in trace
       ("vector: " ++ show vec ++ "| normsqLor vec: " ++ show (normsqLor vec)) $
     norm_max - norm_min

-- contains [GPI set@("Circle", _), P' GPI pt@("", _)] = dist (getX pt, getX pt) (getX set, getY set) - 0.5 * r' set
-- TODO: only approx
-- contains [S' GPI set@("", _), P' GPI pt@("", _)] =
--     dist (getX pt, getX pt) (getX set, getX set) - 0.4 * side' set
-- FIXME: doesn't work
-- contains [E' GPI set@("", _), P' GPI pt@("", _)] =
--     dist (getX pt, getX pt) (xe' set, getX set) - max (rx' set) (ry' set) * 0.9
-- NOTE/HACK: all objects will have min/max size attached, but not all of them are implemented
maxSize :: ConstrFn
-- TODO: why do we need `r2f` now? Didn't have to before
limit = max canvasWidth canvasHeight

maxSize [GPI c@("Circle", _)] = getNum c "r" - r2f (limit / 6)
maxSize [GPI s@("Square", _)] = getNum s "side" - r2f (limit / 3)
maxSize [GPI r@("Rectangle", _)] =
  let max_side = max (getNum r "sizeX") (getNum r "sizeY")
  in max_side - r2f (limit / 3)
maxSize [GPI im@("Image", _)] =
  let max_side = max (getNum im "w") (getNum im "h")
  in max_side - r2f (limit / 3)
maxSize [GPI e@("Ellipse", _)] =
  max (getNum e "rx") (getNum e "ry") - r2f (limit / 6)
maxSize _ = 0

-- NOTE/HACK: all objects will have min/max size attached, but not all of them are implemented
minSize :: ConstrFn
minSize [GPI c@("Circle", _)] = 20 - getNum c "r"
minSize [GPI s@("Square", _)] = 20 - getNum s "side"
minSize [GPI r@("Rectangle", _)] =
  let min_side = min (getNum r "sizeX") (getNum r "sizeY")
  in 20 - min_side
minSize [GPI e@("Ellipse", _)] = 20 - min (getNum e "rx") (getNum e "ry")
minSize [GPI g] =
  if fst g == "Line" || fst g == "Arrow"
    then let vec =
               [ getNum g "endX" - getNum g "startX"
               , getNum g "endY" - getNum g "startY"
               ]
         in 50 - norm vec
    else 0
minSize [GPI g, Val (FloatV len)] =
  if fst g == "Line" || fst g == "Arrow"
    then let vec =
               [ getNum g "endX" - getNum g "startX"
               , getNum g "endY" - getNum g "startY"
               ]
         in len - norm vec
    else 0

smallerThan :: ConstrFn
smallerThan [GPI inc@("Circle", _), GPI outc@("Circle", _)] =
  getNum inc "r" - getNum outc "r" - 0.4 * getNum outc "r" -- TODO: taking this as a parameter?
smallerThan [GPI inc@("Circle", _), GPI outs@("Square", _)] =
  0.5 * getNum outs "side" - getNum inc "r"
smallerThan [GPI ins@("Square", _), GPI outc@("Circle", _)] =
  halfDiagonal $ getNum ins "side" - getNum outc "r"
smallerThan [GPI ins@("Square", _), GPI outs@("Square", _)] =
  getNum ins "side" - getNum outs "side" - subsetSizeDiff

outsideOf :: ConstrFn
outsideOf [GPI l@("Text", _), GPI c@("Circle", _)] =
  let padding = 10.0
  in let labelR = max (getNum l "w") (getNum l "h")
     in -dist (getX l, getY l) (getX c, getY c) + getNum c "r" + labelR +
        padding
-- TODO: factor out runtime weights
outsideOf [GPI l@("Text", _), GPI c@("Circle", _), Val (FloatV weight)] =
  weight * outsideOf [GPI l, GPI c]

overlapping :: ConstrFn
overlapping [GPI xset@("Circle", _), GPI yset@("Circle", _)] =
  looseIntersect
    [ [getX xset, getY xset, getNum xset "r"]
    , [getX yset, getY yset, getNum yset "r"]
    ]
overlapping [GPI xset@("Square", _), GPI yset@("Circle", _)] =
  looseIntersect
    [ [getX xset, getY xset, 0.5 * getNum xset "side"]
    , [getX yset, getY yset, getNum yset "r"]
    ]
overlapping [GPI xset@("Circle", _), GPI yset@("Square", _)] =
  looseIntersect
    [ [getX xset, getY xset, getNum xset "r"]
    , [getX yset, getY yset, 0.5 * getNum yset "side"]
    ]
overlapping [GPI xset@("Square", _), GPI yset@("Square", _)] =
  looseIntersect
    [ [getX xset, getY xset, 0.5 * getNum xset "side"]
    , [getX yset, getY yset, 0.5 * getNum yset "side"]
    ]

looseIntersect :: (Autofloat a) => [[a]] -> a
looseIntersect [[x1, y1, s1], [x2, y2, s2]] =
  dist (x1, y1) (x2, y2) - (s1 + s2 - 10)

disjoint :: ConstrFn
disjoint [GPI xset@("Circle", _), GPI yset@("Circle", _)] =
  noIntersect
    [ [getX xset, getY xset, getNum xset "r"]
    , [getX yset, getY yset, getNum yset "r"]
    ]
-- This is not totally correct since it doesn't account for the diagonal of a square
disjoint [GPI xset@("Square", _), GPI yset@("Square", _)] =
  noIntersect
    [ [getX xset, getY xset, 0.5 * getNum xset "side"]
    , [getX yset, getY yset, 0.5 * getNum yset "side"]
    ]
disjoint [GPI xset@("Rectangle", _), GPI yset@("Rectangle", _), Val (FloatV offset)]
    -- Arbitrarily using x size
 =
  noIntersectOffset
    [ [getX xset, getY xset, 0.5 * getNum xset "sizeX"]
    , [getX yset, getY yset, 0.5 * getNum yset "sizeX"]
    ]
    offset
disjoint [GPI box@("Text", _), GPI seg@("Line", _), Val (FloatV offset)] =
  let center = (getX box, getY box)
      (v, w) = (getPoint "start" seg, getPoint "end" seg)
      cp = closestpt_pt_seg center (v, w)
      len_approx = getNum box "w" / 2.0 -- TODO make this more exact
  in -(dist center cp) + len_approx + offset
    -- i.e. dist from center of box to closest pt on line seg is greater than the approx distance between the box center and the line + some offset
-- For horizontally collinear line segments only
-- with endpoints (si, ei), assuming si < ei (e.g. enforced by some other constraint)
-- Make sure the closest endpoints are separated by some padding
disjoint [GPI o1, GPI o2] =
  if linelike o1 && linelike o2
    then let (start1, end1, start2, end2) =
               ( fst $ getPoint "start" o1
               , fst $ getPoint "end" o1
               , fst $ getPoint "start" o2
               , fst $ getPoint "end" o2 -- Throw away y coords
                )
             padding = 30 -- should be > 0
            -- Six cases for two intervals: disjoint [-] (-), overlap (-[-)-], contained [-(-)-], and swapping the intervals
            -- Assuming si < ei, we can just push away the closest start and end of the two intervals
             distA = unsignedDist end1 start2
             distB = unsignedDist end2 start1
         in if distA <= distB
              then end1 + padding - start2 -- Intervals separated by padding (original condition: e1 + c < s2)
              else end2 + padding - start1 -- e2 + c < s1
    else error "expected two linelike GPIs in `disjoint`"
  where
    unsignedDist :: (Autofloat a) => a -> a -> a
    unsignedDist x y = abs $ x - y

-- exterior point method constraint: no intersection (meaning also no subset)
noIntersect :: (Autofloat a) => [[a]] -> a
noIntersect [[x1, y1, s1], [x2, y2, s2]] =
  -(dist (x1, y1) (x2, y2)) + s1 + s2 + offset
  where
    offset = 10

noIntersectOffset :: (Autofloat a) => [[a]] -> a -> a
noIntersectOffset [[x1, y1, s1], [x2, y2, s2]] offset =
  -(dist (x1, y1) (x2, y2)) + s1 + s2 + offset

atDistFn :: (Autofloat a) => Pt2 a -> Shape a -> a -> a
atDistFn oPt txt offset
  -- TODO: also account for boundary/radius of `o`, rather than just using center
 =
  let ([textPts], _, textBbox, _) = getPolygon txt
  in if isInB' textPts oPt -- The point is inside the box, so push it outside
       then noIntersect
              [[getX txt, getY txt, getNum txt "w"], [fst oPt, snd oPt, 2.0]] -- TODO use better sizes for each object
       else let dsq_res = dsqBP textPts oPt -- Note this does NOT use the signed distance
                constrEnergy = equal' dsq_res (offset * offset)
             {- trace ("\n\ndsq_res: " ++ show dsq_res ++
                    "\nconstrEnergy: " ++ show constrEnergy) -}
            in constrEnergy

-- Uses the closest distance between object center and text bbox
atDist :: ConstrFn
atDist [GPI o, GPI txt@("Text", _), Val (FloatV offset)] =
  atDistFn (getX o, getY o) txt offset
atDist [Val (TupV p), GPI txt@("Text", _), Val (FloatV offset)] =
  atDistFn p txt offset

-- If the point is in the blob, it should have a penalty. If the point is outside the blob, ignore it.
-- TODO: Should we use a bbox on curve to accelerate queries?
polyPtDisjoint :: Autofloat a => Blob a -> Pt2 a -> a
polyPtDisjoint b p = max (-1 * signedDsqBP b p) 0

labelDisjointConstr :: ConstrFn
labelDisjointConstr [GPI curve@("Curve", _), GPI lab@("Text", _), Val (FloatV padding)] =
  let curvePts = polyPts $ getPolygon curve -- TODO: maybe we should re-polygonize the curve instead of using the original points, which are equally distributed in hyperbolic space but not 2D space
      ([textPts], _, textBbox, _) = getPolygon lab
        -- numCurvePts = length curvePts
      sumEnergies = sum $ map (polyPtDisjoint textPts) curvePts
       {- trace ("\nsumEnergies: " ++ show sumEnergies ++
              "\nnumCurvePts: " ++ show numCurvePts) -}
  in sumEnergies

perpendicularConstr :: ConstrFn
perpendicularConstr [Val (TupV q), Val (TupV p), Val (TupV r)] =
  let v1 = q -: p
      v2 = r -: p
      dotprod = v1 `dotv` v2
  in equal' dotprod 0
