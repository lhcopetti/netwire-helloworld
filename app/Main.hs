{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding ((.), id)

import Data.Either
import Control.Monad.Fix (MonadFix)
import Control.Wire
import FRP.Netwire
import Data.Maybe (fromMaybe)
import qualified Data.Bifunctor as Bi (bimap, first, second)
import qualified Graphics.UI.SDL as SDL

data Direction = DLeft | DRight | DUp | DDown | DNothing
    deriving(Eq, Show)


class HasSize a where
    getSize :: a -> Double

class HasPosition a where
    getPosition :: a -> Position

instance HasSize GameObject where
    getSize = size

instance HasPosition GameObject where
    getPosition = pos

type Position = (Double, Double)
type Velocity = (Double, Double)
data GameObject = GO { pos  :: Position
                     , size :: Double
                     } deriving (Show)

windowSize :: Num a => a
windowSize = fromInteger 400

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode windowSize windowSize 32 [SDL.SWSurface]
    go screen clockSession_ challenge4

go screen s w = do

    evts <- accumEvent

    (ds, s') <- stepSession s
    (ex, w') <- stepWire w ds (Right evts)

    let x' = either (const (GO (0, 0) 50)) id ex
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>= 
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 windowSize >>= do
        let xPos    = round . fst . pos $ x'
        let yPos    = round . snd . pos $ x'
        let goSize  = round . size      $ x'
        SDL.fillRect screen (Just $ SDL.Rect xPos yPos goSize goSize)

    SDL.flip screen
    SDL.delay (1000 `div` 60)
    go screen s' w' 

challenge4 :: (HasTime t s, MonadFix m) => Wire s () m [SDL.Event] GameObject
challenge4 = proc evts -> do
    pressedKeys <- processKeys [] -< evts
    rec
        newPos <- updatePosition <<< second delayGo -< (pressedKeys, newGo)
        newSize <- updateSize -< pressedKeys
        newGo <- toGo -< (newPos, newSize)
    returnA -< newGo


fireAndDelay :: (Monad m, HasTime t s, Monoid e) => MemoryCommand -> Wire s e m a (Maybe MemoryCommand)
fireAndDelay v = mkSFN $ \_ -> 
    let nextWire = (pure Nothing . for 1) --> fireAndDelay v
    in  (Just v, nextWire)

controllablePosition :: (HasTime t s, Monoid e, MonadFix m) => Wire s e m (Maybe MemoryCommand, Velocity) Position
controllablePosition = proc (cc, vel) -> do
    rec
        input <- saveOrRestore <|> arr (Integrate . snd) -< ((cc, newPos), vel)
        newPos <- positionW2 -< input
    returnA -< newPos

saveOrRestore :: (Monoid e, Monad m) => Wire s e m ((Maybe MemoryCommand, Position), Velocity) (IntegralCommand Position)
saveOrRestore = memoryDriver Nothing . arr (Bi.second Set . fst)


delayGo :: Wire s e m GameObject GameObject
delayGo = delay emptyGo

emptyGo :: GameObject
emptyGo = GO (0, 0) 0 


updatePosition :: (Monoid e, HasTime t s, MonadFix m) => Wire s e m ([SDL.Keysym], GameObject) Position
updatePosition = proc (pressedKeys, go) -> do
        direction <- nextDirection -< (go, pressedKeys)
        newPos <- position -< (pressedKeys, direction)
        returnA -< newPos

updateSize :: (HasTime t s, MonadFix m, Monoid e) => Wire s e m [SDL.Keysym] Double
updateSize = getSizeIncrement &&& pure 0 >>> integralWith noNegative 50

noNegative :: Double -> Double -> Double
noNegative _ x
    | x < 0 = 0
    | otherwise = x

getSizeIncrement :: (HasTime t s, MonadFix m, Monoid e) => Wire s e m [SDL.Keysym] Double
getSizeIncrement = (when (`isKeyPressed` SDL.SDLK_p) >>>  pure (25)) <|>
                   (when (`isKeyPressed` SDL.SDLK_m) >>>  pure (-25)) <|>
                   pure 0



toGo :: Monad m => Wire s e m (Position, Double) GameObject
toGo = arr $ uncurry GO

position :: (Monoid e, HasTime t s, MonadFix m) =>  Wire s e m ([SDL.Keysym], Direction) Position
position = controlCounterV *** arr mapSpeed >>> controllablePosition

controlCounterV :: (Monoid e, HasTime t s, Monad m) => Wire s e m [SDL.Keysym] (Maybe MemoryCommand)
controlCounterV =   fireAndDelay MSave . when (`isKeyPressed` SDL.SDLK_a)
                <|> fireAndDelay MGet . when (`isKeyPressed` SDL.SDLK_s)
                <|> (pure Nothing)

positionW :: (Monoid e, HasTime t s, Monad m) => Wire s e m (IntegralCommand Double, IntegralCommand Double) Position
positionW = integralE 75 *** integralE 75

positionW2 :: (Monoid e, HasTime t s, Monad m) => Wire s e m (IntegralCommand Position) Position
positionW2 = let go (Integrate tp) = Bi.bimap Integrate Integrate tp 
                 go (Set       tp) = Bi.bimap Set       Set       tp
             in arr (\tp -> go tp) >>> positionW

mapSpeed :: Direction -> Position
mapSpeed DLeft    = (-150.0, 0)
mapSpeed DRight   = (150.0,  0)
mapSpeed DUp      = (0, -150.0)
mapSpeed DDown    = (0, 150.0 )
mapSpeed DNothing = (0.0, 0.0)


fromEither :: Either a a -> a
fromEither (Left x)  = x
fromEither (Right x) = x

nextDirection :: Monad m => Wire s e m (GameObject, [SDL.Keysym]) Direction
nextDirection = second (arr dirFromInput) >>> selectDirection DNothing

dirFromInput :: [SDL.Keysym] -> Maybe Direction
dirFromInput evts = safeHead Nothing
                        . map (Just . snd)
                        . filter fst
                        . map (Bi.first (isKeyPressed evts)) $ actions

safeHead :: a -> [a] -> a
safeHead x [] = x
safeHead _ (x:_) = x

actions :: [(SDL.SDLKey, Direction)]
actions =  [ (SDL.SDLK_LEFT, DLeft)
           , (SDL.SDLK_RIGHT, DRight)
           , (SDL.SDLK_UP, DUp)
           , (SDL.SDLK_DOWN, DDown)
           , (SDL.SDLK_SPACE, DNothing)
           ] 

selectDirection :: Direction -> Wire s e m (GameObject, Maybe Direction) Direction
selectDirection previousDir = mkSFN $ \(go, dirFromInput) -> 
    let nextInputDir    = fromMaybe previousDir dirFromInput
        forcedNextDir   = shouldForceDirection go
        nextDir         = fromMaybe nextInputDir forcedNextDir
    in  (nextDir, selectDirection nextDir)

shouldForceDirection :: GameObject -> Maybe Direction
shouldForceDirection go = forcedRight go <|> forcedLeft go <|> forcedUp go <|> forcedDown go

forcedRight :: HasPosition a => a -> Maybe Direction
forcedRight x
    | (fst . getPosition) x < 0 = Just DRight
    | otherwise = Nothing

forcedLeft :: (HasSize a, HasPosition a) => a -> Maybe Direction
forcedLeft x
    | (fst . getPosition) x + getSize x > windowSize = Just DLeft
    | otherwise = Nothing

forcedUp :: (HasSize a, HasPosition a) => a -> Maybe Direction
forcedUp y
    | (snd . getPosition) y + getSize y > windowSize = Just DUp
    | otherwise = Nothing

forcedDown :: HasPosition a => a -> Maybe Direction
forcedDown y
    | (snd . getPosition) y < 0 = Just DDown
    | otherwise = Nothing


isKeyPressed :: [SDL.Keysym] -> SDL.SDLKey -> Bool
isKeyPressed xs key = not . null . filter (== key) . map SDL.symKey $ xs

processKeys :: Monad m => [SDL.Keysym] -> Wire s e m [SDL.Event] [SDL.Keysym]
processKeys keys = mkSFN $ \evts ->
    let keys' = foldr foldEvents keys evts
    in  (keys', processKeys keys')


foldEvents :: SDL.Event -> [SDL.Keysym] -> [SDL.Keysym]
foldEvents (SDL.KeyDown k) = (k:)
foldEvents (SDL.KeyUp k) = filter (/= k)
foldEvents _ = id 


accumEvent :: IO [SDL.Event]
accumEvent = go []
    where
        go xs = do
            evt <- SDL.pollEvent
            case evt of
                SDL.NoEvent -> return xs
                evt' -> go (evt':xs)


------------ Conditional Integral ------------
data IntegralCommand a = Integrate a | Set a

integralE :: (Monad m, Monoid e, Fractional a, HasTime t s) => a -> Wire s e m (IntegralCommand a) a
integralE start = 
    integral start . arr integralCommandValue . when isIntegrate
    --> mkSFN (\epos -> 
        let newValue = integralCommandValue epos
        in  (newValue, integralE newValue))

isIntegrate :: IntegralCommand a -> Bool
isIntegrate (Integrate _) = True
isIntegrate (Set       _) = False

integralCommandValue :: IntegralCommand a -> a
integralCommandValue (Integrate v) = v
integralCommandValue (Set       v) = v


------------ Memory Wire ------------
data MemoryCommand = MSave | MGet

memoryDriver :: Monoid e => Maybe a -> Wire s e m (Maybe MemoryCommand, a) a
memoryDriver mMemory = mkPureN $ \commAndValue ->
    let driver _         (Just MSave, value) = (Just value, Left mempty)
        driver (Just x)  (Just MGet, _)      = (Just x    , Right x)
        driver stored    _                   = (stored    , Left mempty)

        (newmMemory, result) = driver mMemory commAndValue
    in  (result, memoryDriver newmMemory)
