module Main where

import Reflex.Dom

import Lib
import PositionsWidget

main = let (throwPattern, movementPattern) = shootingStar
           bodies = movementPatternBodies movementPattern
           scripts = scriptsFromBodies bodies
           bounds = extremities bodies
        in mainWidget $ do
             positionsWidget scripts 12 bounds 0
             positionsWidget scripts 12 bounds 1
             positionsWidget scripts 12 bounds 2
             positionsWidget scripts 12 bounds 3
             positionsWidget scripts 12 bounds 4
