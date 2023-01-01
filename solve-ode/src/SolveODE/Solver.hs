{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module SolveODE.Solver (solveByEuler, solveByRK44) where

import Numeric.LinearAlgebra ( Linear(scale) )
import SolveODE.Types ( Solver, ODE, Vec(..) )
import Data.List ( unfoldr )

solveByEuler :: Double -> ODE -> (Vec -> Bool) -> Vec -> [Vec]
solveByEuler dt = solveODE $ euler dt

solveByRK44 :: Double -> ODE -> (Vec -> Bool) -> Vec -> [Vec]
solveByRK44 dt = solveODE $ rungeKutta44 dt


solveODE :: Solver -> ODE -> (Vec -> Bool) -> Vec -> [Vec]
solveODE solver ode endP = unfoldr (\vec -> let next = solver ode vec
                                            in if endP next
                                               then Nothing
                                               else Just (vec, next))

euler :: Double -> Solver
euler dt = \ode v -> let x' = (ode v).x
                         t' = v.t + dt
                         v' = v.x + dt `scale` x'
                     in V t' v'

rungeKutta44 :: Double -> Solver
rungeKutta44 dt = \ode v -> let t_ = v.t +0.5*dt
                                t' = v.t     +dt
                                k1 = (ode v).x
                                k2 = (ode $ V t_ $ v.x +(0.5*dt) `scale` k1).x
                                k3 = (ode $ V t_ $ v.x +(0.5*dt) `scale` k2).x
                                k4 = (ode $ V t' $ v.x       +dt `scale` k3).x
                                v' = v.x +(dt/6.0) `scale` (k1 +2.0*k2 +2.0*k3 +k4)
                            in V t' v'
