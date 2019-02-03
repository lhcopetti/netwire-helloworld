{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding ((.), id)
import qualified Prelude as P ((.), id)

import Control.Monad.Fix (MonadFix)
import qualified Control.Monad as MN
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Wire
import FRP.Netwire
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import qualified Graphics.UI.SDL as SDL

data ADirection = DLeft | DRight | DUp | DDown | DNothing
    deriving(Eq, Show)

data MemoryCommand = MSave | MRestore

class HasSize a where
    getSize :: a -> Double

class HasPosition a where
    getPosition :: a -> Position

instance HasSize GameObject where
    getSize = size

instance HasPosition GameObject where
    getPosition = pos


main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
    go screen clockSession_ challenge4

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
        input <- saveOrRestore <|> mkSF_ (Right P.. snd) -< ((cc, newPos), vel)
        newPos <- positionW2 -< input
    returnA -< newPos

saveOrRestore :: (Monoid e, Monad m) => Wire s e m ((Maybe MemoryCommand, Position), Velocity) (Either Position Position)
saveOrRestore = arr Left . cachePositionW Nothing . mkSF_ fst


cachePositionW :: Monoid e => Maybe Position -> Wire s e m (Maybe MemoryCommand, Position) Position
cachePositionW mPos = mkPureN $ \ccAndPos ->
    let (newMPos, result) = cachePosition mPos ccAndPos
    in (result, cachePositionW newMPos)

cachePosition :: Monoid e => Maybe Position -> (Maybe MemoryCommand, Position) -> (Maybe Position, Either e Position)
cachePosition _         (Just MSave, pos )  = (Just pos, Left mempty)
cachePosition (Just x)  (Just MRestore, _)  = (Just x  , Right x)
cachePosition stored    _                   = (stored  , Left mempty)
    
mapInputControls :: (MemoryCommand, Position) -> Either Position Position
mapInputControls (_, x)  = Right x


type Position = (Double, Double)
type Velocity = (Double, Double)
data GameObject = GO { pos  :: Position
                     , size :: Double
                     } deriving (Show)

go screen s w = do

    evts <- accumEvent

    (ds, s') <- stepSession s
    (ex, w') <- stepWire w ds (Right evts)

    let x' = either (const (GO (0, 0) 50)) id ex
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>= 
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= do
        let xPos    = round . fst . pos $ x'
        let yPos    = round . snd . pos $ x'
        let goSize  = round . size      $ x'
        SDL.fillRect screen (Just $ SDL.Rect xPos yPos goSize goSize)

    SDL.flip screen
    SDL.delay (1000 `div` 60)
    go screen s' w' 


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
updateSize = getSizeIncrement &&& pure 0 >>> integralWith noNegative' 50

noNegative' :: Double -> Double -> Double
noNegative' _ = noNegative

noNegative :: Double -> Double
noNegative x
    | x < 0 = 0
    | otherwise = x

getSizeIncrement :: (HasTime t s, MonadFix m, Monoid e) => Wire s e m [SDL.Keysym] Double
getSizeIncrement = (when (`isKeyPressed` SDL.SDLK_p) >>>  pure (25)) <|>
                   (when (`isKeyPressed` SDL.SDLK_m) >>>  pure (-25)) <|>
                   pure 0



toGo :: Monad m => Wire s e m (Position, Double) GameObject
toGo = arr $ uncurry GO

controlCounterV :: (Monoid e, HasTime t s, Monad m) => Wire s e m [SDL.Keysym] (Maybe MemoryCommand)
controlCounterV =   fireAndDelay MSave . when (`isKeyPressed` SDL.SDLK_a)
                <|> fireAndDelay MRestore . when (`isKeyPressed` SDL.SDLK_s)
                <|> (pure Nothing)

position :: (Monoid e, HasTime t s, MonadFix m) =>  Wire s e m ([SDL.Keysym], ADirection) Position
position = controlCounterV *** arr mapSpeed >>> controllablePosition


positionW :: (HasTime t s, Monad m) => Wire s e m (Either Double Double, Either Double Double) Position
positionW = integralE 75 *** integralE 75

positionW2 :: (HasTime t s, Monad m) => Wire s e m (Either Position Position) Position
positionW2 = let go (Left tp) = bimap Left Left tp 
                 go (Right tp) = bimap Right Right tp
             in mkSF_ (\tp -> go tp) >>> positionW



data HnF = HActivate | HDont
                deriving (Show)

-- holdAndFire :: 
holdAndFire :: Maybe b -> (a -> b) -> (a -> b) -> Wire s e m (HnF, a) b
holdAndFire current onActive onNothing = mkSFN $ \(hnf, a) ->
    let (newAcc, res) = holdAndFire' current onActive onNothing hnf a
    in (res, holdAndFire newAcc onActive onNothing)


holdAndFire' :: Maybe b -> (a -> b) -> (a -> b) -> HnF -> a -> (Maybe b, b)
holdAndFire' m         _            onNothing HDont     x = (m, onNothing x)
holdAndFire' Nothing   onActivate   onNothing HActivate x = (Just (onActivate x), onNothing x) 
holdAndFire' (Just x)  _            _         HActivate _ = (Nothing, x) 

mapSpeed :: ADirection -> Position
mapSpeed DLeft    = (-150.0, 0)
mapSpeed DRight   = (150.0,  0)
mapSpeed DUp      = (0, -150.0)
mapSpeed DDown    = (0, 150.0 )
mapSpeed DNothing = (0.0, 0.0)

integralE :: (Fractional a, HasTime t s) => a -> Wire s e m (Either a a) a
integralE x = mkSF $ \ds dx -> let
    newValue = integralE' x ds dx
    in (newValue, integralE newValue)
--mkSF $ \ds dx -> do
    --let dt = realToFrac (dtime ds)
    --in undefined

integralE' :: (Fractional a, HasTime t s) => a -> s -> Either a a -> a
integralE' _ _ (Left x) = x
integralE' x ds (Right dx) = let
    dt = realToFrac (dtime ds)
    in x + dt * dx


nextDirection :: Monad m => Wire s e m (GameObject, [SDL.Keysym]) ADirection
nextDirection = second (mkSF_ dirFromInput) >>> selectDirection DNothing

dirFromInput :: [SDL.Keysym] -> Maybe ADirection
dirFromInput evts =  
    let resolve [] = Nothing
        resolve xs = Just P.. snd P.. head $ xs
    in  resolve 
            P.. filter (\ (p, _) -> p) 
            P.. overFst (isKeyPressed evts) $ actions

overFst :: (a -> c) -> [(a, b)] -> [(c, b)]
overFst f = liftA2 zip (map (f . fst)) (map snd)

actions :: [(SDL.SDLKey, ADirection)]
actions =  [ (SDL.SDLK_LEFT, DLeft)
           , (SDL.SDLK_RIGHT, DRight)
           , (SDL.SDLK_UP, DUp)
           , (SDL.SDLK_DOWN, DDown)
           , (SDL.SDLK_SPACE, DNothing)
           ] 

selectDirection :: ADirection -> Wire s e m (GameObject, Maybe ADirection) ADirection
selectDirection previousDir = mkSFN $ \(go, dirFromInput) -> 
    let nextInputDir    = fromMaybe previousDir dirFromInput
        forcedNextDir   = shouldForceDirection go
        nextDir         = fromMaybe nextInputDir forcedNextDir
    in  (nextDir, selectDirection nextDir)

shouldForceDirection :: GameObject -> Maybe ADirection
shouldForceDirection go = forcedRight go <|> forcedLeft go <|> forcedUp go <|> forcedDown go

forcedRight :: HasPosition a => a -> Maybe ADirection
forcedRight x
    | (fst . getPosition) x < 0 = Just DRight
    | otherwise = Nothing

forcedLeft :: (HasSize a, HasPosition a) => a -> Maybe ADirection
forcedLeft x
    | (fst . getPosition) x + getSize x > 200 = Just DLeft
    | otherwise = Nothing

forcedUp :: (HasSize a, HasPosition a) => a -> Maybe ADirection
forcedUp y
    | (snd . getPosition) y + getSize y > 200 = Just DUp
    | otherwise = Nothing

forcedDown :: HasPosition a => a -> Maybe ADirection
forcedDown y
    | (snd . getPosition) y < 0 = Just DDown
    | otherwise = Nothing


isKeyPressed :: [SDL.Keysym] -> SDL.SDLKey -> Bool
isKeyPressed xs key = not P.. null P.. filter (== key) P.. map SDL.symKey $ xs

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

