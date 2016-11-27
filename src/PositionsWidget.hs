module PositionsWidget where

import           Control.Lens
import           Data.List (minimumBy, maximumBy)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Text as T
import           Reflex.Dom

import           Lib


adjustPoint (x, y) bounds width height = (x' * width, y' * height)
  where
    x' = (x - bounds ^. xmin) / (bounds ^. xmax - bounds ^. xmin)
    y' = (y - bounds ^. ymin) / (bounds ^. ymax - bounds ^. ymin)

positionsWidget :: (Reflex t, MonadWidget t m) => Map JugglerId [(Int, Position)] -> Int -> Bounds -> Double -> m () 
positionsWidget scripts duration bounds time = elAttr "div" (Map.singleton "style" "width: 200px; height: 200px; position: relative; border: 1px solid blue; float: left; margin: 2em;") $ flip mapM_ (Map.assocs scripts) $ \(jugglerId, script) ->
    let (t1,p1) = positionBefore (floor time) script duration
        (t2,p2) = positionAfter (ceiling time) script duration
        point = interpolate ((time - fromIntegral t1) / (fromIntegral $ t2 - t1)) p1 p2
        (x, y) = adjustPoint point bounds 200 200
        css = "left: " ++ show x ++ "px; top: " ++ show y ++ "px; position: absolute;"
        attrs = constDyn ("style" =: css)
     in elDynAttr "div" attrs $ Reflex.Dom.text (T.unpack $ jugglerIdText jugglerId)


compareScriptElementTimes :: ScriptElement -> ScriptElement -> Ordering
compareScriptElementTimes a b = fst a `compare` fst b

positionBefore :: Int -> [ScriptElement] -> Int -> ScriptElement
positionBefore time script duration = case [(t, p) | (t,p) <- script, t < time] of
                                        [] -> over _1 ((-) duration) $ maximumBy compareScriptElementTimes script
                                        before -> maximumBy compareScriptElementTimes before

positionAfter :: Int -> [ScriptElement] -> Int -> ScriptElement
positionAfter time script duration = case [(t, p) | (t,p) <- script, t > time] of
                                        [] -> over _1 (+ duration) $ minimumBy compareScriptElementTimes script
                                        before -> minimumBy compareScriptElementTimes before

interpolate :: Double -> Position -> Position -> (Double, Double)
interpolate distance p1 p2 = (((p2 ^. x) - (p1 ^. x)) * distance + (p1 ^. x)
                             ,((p2 ^. y) - (p1 ^. y)) * distance + (p1 ^. y)
                             )
