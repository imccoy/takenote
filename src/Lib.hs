module Lib where

import           Control.Lens
import           Data.Functor.Foldable
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype RoleId = RoleId Text
  deriving (Show, IsString, Ord, Eq)

roleIdText (RoleId t) = t

newtype ArrangementId = ArrangementId Text
  deriving (IsString, Ord, Eq)
newtype JugglerId = JugglerId Text
  deriving (Show, IsString, Ord, Eq)

jugglerIdText (JugglerId t) = t

data Direction = LeftRight !LeftRight
               | ForwardBackward !ForwardBackward
data LeftRight = Left | Right
data ForwardBackward = Forward | Backward

data Position = Position { _x :: !Double, _y :: !Double, _angle :: !Double }
  deriving (Show, Ord, Eq)
makeLenses ''Position

adjustPosition :: Direction -> Double -> Position -> Position
adjustPosition (LeftRight Lib.Left) = adjustPosition' (-1) 0
adjustPosition (LeftRight Lib.Right) = adjustPosition' 1 0
adjustPosition (ForwardBackward Forward) = adjustPosition' 0 1
adjustPosition (ForwardBackward Backward) = adjustPosition' 0 (-1)

adjustPosition' :: Double -> Double -> Double -> Position -> Position
adjustPosition' xdistance ydistance distance position = let angle' = (position ^. angle) * pi / 180
                                                            x' = distance * (xdistance * cos angle' + ydistance * sin angle')
                                                            y' = distance * (xdistance * sin angle' + ydistance * cos angle')
                                                         in x +~ x' $ y +~ y' $ position


data LocationF l = Fixed !Position
                 | Relative !Direction !Double l
                 | Twist !Double l
                 | Between l l
                 | Facing l
                 | Circle Int LeftRight l
  deriving (Functor)
type Location = Fix LocationF

fixed = Fix . Fixed

relative :: Direction -> Double -> Location -> Location
relative direction distance = Fix . Relative direction distance

twist :: Double -> Location -> Location
twist angle = Fix . Twist angle

between :: Location -> Location -> Location
between a b = Fix $ Between a b

facing :: Location -> Location
facing = Fix . Facing

circle :: Int -> LeftRight -> Location -> Location
circle circumference direction = Fix . Circle circumference direction

positionFromLocation = fold $ \case
  (Fixed p)                           -> p
  (Relative direction distance l)     -> adjustPosition direction distance l
  (Twist angle' l)                    -> angle +~ angle' $ l
  (Between p1 p2)                     -> Position (p1 ^. x + p2 ^. x / 2)
                                                  (p1 ^. y + p2 ^. y / 2)
                                                  (p1 ^. angle + p2 ^. angle / 2)
  (Facing l)                          -> positionFromLocation $ relative (ForwardBackward Forward) 2 . 
                                                                  twist 180
                                                                  $ fixed l
  (Circle numberInCircle leftRight l) -> positionFromLocation $ twist (interiorAngle / 2) . 
                                                                  relative (LeftRight leftRight) 1 .
                                                                  twist (interiorAngle / 2)
                                                                  $ fixed l
                                           where interiorAngle = 360 / (fromIntegral numberInCircle) -- the angle between two jugglers in the "circle"


type PatternBeats hand = Vector [(hand, hand)]

data ThrowPattern = VanillaPattern !(PatternBeats RoleId)
                  | SyncPattern !(PatternBeats (RoleId, LeftRight))

newtype Arrangement = Arrangement (Map RoleId Location)
newtype Arrangements = Arrangements (Vector Arrangement)

data MovementPattern = Stationary !Arrangement
                     | Looping !Arrangements
                     | Rotating !Arrangements
                                !(Map RoleId RoleId) -- who becomes who

type Pattern = (ThrowPattern, MovementPattern)

shootingStar = (VanillaPattern $ Vector.fromList $ replicate 3 [("A", "B"),("B","C"),("C","D")],
                Rotating (Arrangements arrangements)
                         (Map.fromList [("A","D"),("D","C"),("C","B"),("B","A")]))
  where arrangements = let circle2 = circle 5 Lib.Right . circle 5 Lib.Right
                           aPos = fixed $ Position 0 0 0
                           bPos = circle2 aPos
                           cPos = circle2 bPos
                           dPos = circle2 cPos
                           arrangement = Arrangement $ Map.fromList [("A", aPos)
                                                                    ,("B", bPos)
                                                                    ,("C", cPos)
                                                                    ,("D", dPos)
                                                                    ]
                           endArrangement = Arrangement $ Map.fromList [("A", circle2 dPos)
                                                                       ,("B", bPos)
                                                                       ,("C", cPos)
                                                                       ,("D", dPos)
                                                                       ]
                        in Vector.fromList $ replicate 3 arrangement ++ [endArrangement]

positionsFromArrangement (Arrangement arrangement) = Map.mapKeys (JugglerId . roleIdText) . Map.map positionFromLocation $ arrangement

movementPatternBodies :: MovementPattern -> Vector (Map JugglerId Position)
movementPatternBodies (Stationary arrangement) = Vector.singleton $ positionsFromArrangement arrangement
movementPatternBodies (Looping (Arrangements arrangements)) = Vector.map positionsFromArrangement arrangements
movementPatternBodies (Rotating (Arrangements arrangements) nextRoles) = Vector.concat . map locationsForRoleAssignment $ roleAssignments
  where locationsForRoleAssignment roleAssignment = Vector.map (Map.map positionFromLocation . arrangementForRoleAssignment roleAssignment) arrangements
        arrangementForRoleAssignment roleAssignment (Arrangement arrangement) = Map.fromList . catMaybes . map (\(k, role) -> (k,) <$> Map.lookup role arrangement) . Map.toList $ roleAssignment
        initialRoles = Map.mapKeys (JugglerId . roleIdText) $ Map.mapWithKey (\k _ -> k) $ Map.unions $ map (\(Arrangement m) -> m) $ Vector.toList arrangements
        roleAssignments = initialRoles:(takeWhile (/= initialRoles) $ drop 1 $ iterate advanceRoles initialRoles)
        advanceRoles roles = Map.map (\role -> fromMaybe role $ Map.lookup role nextRoles) roles

movementPatternBodies' (Rotating (Arrangements arrangements) nextRoles) = roleAssignments
  where locationsForRoleAssignment roleAssignment = Vector.map (Map.map positionFromLocation . arrangementForRoleAssignment roleAssignment) arrangements
        arrangementForRoleAssignment roleAssignment (Arrangement arrangement) = Map.fromList . catMaybes . map (\(k, role) -> (k,) <$> Map.lookup role arrangement) . Map.toList $ roleAssignment
        initialRoles = Map.mapKeys (JugglerId . roleIdText) $ Map.mapWithKey (\k _ -> k) $ Map.unions $ map (\(Arrangement m) -> m) $ Vector.toList arrangements
        roleAssignments = initialRoles:(takeWhile (/= initialRoles) $ drop 1 $ iterate advanceRoles initialRoles)
        advanceRoles roles = Map.map (\role -> fromMaybe role $ Map.lookup role nextRoles) roles



type ScriptElement = (Int, Position)

scriptsFromBodies :: Vector (Map JugglerId Position) -> Map JugglerId [ScriptElement]
scriptsFromBodies = Vector.ifoldr (\time positions map -> 
                                         Map.foldrWithKey (\jugglerId position -> 
                                                              Map.alter (Just . ((time, position):) . fromMaybe []) jugglerId
                                                          )
                                                          map
                                                          positions
                                  )
                                  Map.empty

data Bounds = Bounds { _xmin :: Double
                     , _ymin :: Double
                     , _xmax :: Double
                     , _ymax :: Double
                     }
  deriving (Show)

makeLenses ''Bounds

extremities :: Vector (Map JugglerId Position) -> Bounds
extremities = List.foldl' includePosition (Bounds 0 0 0 0) . (concat . fmap Map.elems . Vector.toList)
  where includePosition bounds position = Bounds (min (bounds ^. xmin) (position ^. x))
                                                 (min (bounds ^. ymin) (position ^. y))
                                                 (max (bounds ^. xmax) (position ^. x))
                                                 (max (bounds ^. ymax) (position ^. y))

