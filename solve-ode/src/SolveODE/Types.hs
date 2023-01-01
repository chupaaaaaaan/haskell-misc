{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SolveODE.Types
    ( Vec(..)
    , ODE
    , Solver
    , mkVec
    , module Numeric.LinearAlgebra
) where

import Numeric.LinearAlgebra

data Vec =  V { t :: Double
              , x :: Vector Double
              }

instance Show Vec where
    show v = show v.t ++ concatMap ((" "++) . show) (toList v.x)

mkVec :: Double -> [Double] -> Vec
mkVec t = V t . vector

type ODE = Vec -> Vec
type Solver = ODE -> Vec -> Vec

