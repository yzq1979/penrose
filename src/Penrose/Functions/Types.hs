{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}

module Penrose.Functions.Types where

import qualified Data.Map.Strict as M
import qualified Data.MultiMap   as MM
import           Penrose.Shapes
import           Penrose.Util
import           System.Random   (StdGen, mkStdGen, randomR)

--------------------------------------------------------------------------------
type FuncName = String

type OptSignatures = MM.MultiMap String [ArgType]

-- TODO: should computations be overloaded?
type CompSignatures = M.Map String ([ArgType], ArgType)

type OptFn a = [ArgVal a] -> a

type ObjFnOn a = [ArgVal a] -> a

type ConstrFnOn a = [ArgVal a] -> a

type CompFnOn a = [ArgVal a] -> StdGen -> (ArgVal a, StdGen)

type ObjFn
   = forall a. (Autofloat a) =>
                 [ArgVal a] -> a

type ConstrFn
   = forall a. (Autofloat a) =>
                 [ArgVal a] -> a

type CompFn
   = forall a. (Autofloat a) =>
                 [ArgVal a] -> StdGen -> (ArgVal a, StdGen)

-- | computations that do not use randomization
type ConstCompFn
   = forall a. (Autofloat a) =>
                 [ArgVal a] -> ArgVal a

-- TODO: are the Info types still needed?
type Weight a = a

type ObjFnInfo a = (ObjFnOn a, Weight a, [Value a])

type ConstrFnInfo a = (ConstrFnOn a, Weight a, [Value a])

data FnInfo a
  = ObjFnInfo a
  | ConstrFnInfo a

-- For a very limited form of supertyping...
linelike :: Autofloat a => Shape a -> Bool
linelike shape = fst shape == "Line" || fst shape == "Arrow"

linePts, arrowPts :: (Autofloat a) => Shape a -> (a, a, a, a)
linePts = arrowPts

arrowPts a =
  (getNum a "startX", getNum a "startY", getNum a "endX", getNum a "endY")
