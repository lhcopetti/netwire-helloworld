{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.), id)

import Data.Either
import Control.Monad.Fix (MonadFix)
import Control.Wire
import FRP.Netwire
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.Bifunctor as Bi (bimap, first, second)
import qualified Graphics.UI.SDL as SDL

data Direction = DLeft | DRight | DUp | DDown | DNothing
    deriving (Eq, Show)


class HasSize a where
    getSize :: a -> Double

class HasPosition a where
    getPosition :: a -> Position

    getX :: a -> Double
    getX = fst . getPosition

    getY :: a -> Double
    getY = snd . getPosition

instance HasSize Square where
    getSize = size

instance HasPosition Square where
    getPosition = pos

type Position = (Double, Double)
type Velocity = (Double, Double)



------------ Square ------------
data Square = Sq { pos  :: Position
                 , size :: Double
                 } deriving (Show)

delaySquare :: Wire s e m Square Square
delaySquare = delay emptySquare

emptySquare :: Square
emptySquare = Sq (0, 0) 0 

toSquare :: Monad m => Wire s e m (Position, Double) Square
toSquare = arr (uncurry Sq)

------------ Constants ------------
windowSize :: Num a => a
windowSize = 400

blockVelocity :: Double
blockVelocity = 300.0

blockInitialSize :: Double
blockInitialSize = 75

blockChangeSizeSpeed :: Double
blockChangeSizeSpeed = 200

------------ Main ------------
main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode windowSize windowSize 32 [SDL.SWSurface]
    go screen clockSession_ challenge4

go screen s w = do

    evts <- accumEvent

    (ds, s') <- stepSession s
    (ex, w') <- stepWire w ds (Right evts)

    let x' = either (const (Sq (0, 0) blockInitialSize)) id ex
    clearScreen screen (255, 255, 255)
    drawSquare screen (0, 50, 100) x'

    SDL.flip screen
    SDL.delay (1000 `div` 60)
    go screen s' w' 


challenge4 :: (HasTime t s, MonadFix m) => Wire s () m [SDL.Event] Square
challenge4 = proc evts -> do
    pressedKeys <- processKeys [] -< evts
    rec
        newPos <- updatePosition <<< second delaySquare -< (pressedKeys, newGo)
        newSize <- updateSize -< pressedKeys
        newGo <- toSquare -< (newPos, newSize)
    returnA -< newGo

updateSize :: (HasTime t s, MonadFix m, Monoid e) => Wire s e m [SDL.Keysym] Double
updateSize = getSizeIncrement &&& pure 0 >>> integralWith noNegative blockInitialSize

getSizeIncrement :: (HasTime t s, MonadFix m, Monoid e) => Wire s e m [SDL.Keysym] Double
getSizeIncrement =    (when (`isKeyPressed` SDL.SDLK_p) >>>  pure blockChangeSizeSpeed)
                  <|> (when (`isKeyPressed` SDL.SDLK_m) >>>  pure (-blockChangeSizeSpeed))
                  <|> pure 0

noNegative :: Double -> Double -> Double
noNegative _ x
    | x < 0 = 0
    | otherwise = x


updatePosition :: (Monoid e, HasTime t s, MonadFix m) =>  Wire s e m ([SDL.Keysym], Square) Position
updatePosition = (fst ^>> mapInputTeleport) &&& mapInputSpeed >>> resolvePosition

mapInputTeleport :: (Monoid e, HasTime t s, Monad m) => Wire s e m [SDL.Keysym] (Maybe MemoryCommand)
mapInputTeleport =   fireAndDelay MSave . when (`isKeyPressed` SDL.SDLK_a)
                <|> fireAndDelay MGet . when (`isKeyPressed` SDL.SDLK_s)
                <|> pure Nothing

fireAndDelay :: (Monad m, HasTime t s, Monoid e) => MemoryCommand -> Wire s e m a (Maybe MemoryCommand)
fireAndDelay v = mkSFN $ \_ -> 
    let nextWire = (pure Nothing . for 1) --> fireAndDelay v
    in  (Just v, nextWire)

mapInputSpeed :: (Monoid e, HasTime t s, MonadFix m) =>  Wire s e m ([SDL.Keysym], Square) Velocity
mapInputSpeed =
    let speed DLeft    = (-blockVelocity, 0)
        speed DRight   = (blockVelocity,  0)
        speed DUp      = (0, -blockVelocity)
        speed DDown    = (0, blockVelocity )
        speed DNothing = (0.0, 0.0)
    in  nextDirection >>^ speed
        

resolvePosition :: (HasTime t s, Monoid e, MonadFix m) => Wire s e m (Maybe MemoryCommand, Velocity) Position
resolvePosition = proc (memCommand, vel) -> do
    rec
        input <- saveOrRestorePosition <|> integrateVelocity -< ((memCommand, newPos), vel)
        newPos <- positionW2 -< input
    returnA -< newPos

saveOrRestorePosition :: (Monoid e, Monad m) => Wire s e m ((Maybe MemoryCommand, Position), Velocity) (IntegralCommand Position)
saveOrRestorePosition = Bi.second Set . fst ^>> memoryDriver Nothing  

integrateVelocity :: Monad m => Wire s e m ((Maybe MemoryCommand, Position), Velocity) (IntegralCommand Velocity)
integrateVelocity = arr (Integrate . snd)

positionW2 :: (Monoid e, HasTime t s, Monad m) => Wire s e m (IntegralCommand Position) Position
positionW2 = let go (Integrate tp) = Bi.bimap Integrate Integrate tp 
                 go (Set       tp) = Bi.bimap Set       Set       tp
             in go ^>> positionW

positionW :: (Monoid e, HasTime t s, Monad m) => Wire s e m (IntegralCommand Double, IntegralCommand Double) Position
positionW = integralE 75 *** integralE 75

nextDirection :: Monad m => Wire s e m ([SDL.Keysym], Square) Direction
nextDirection = first (arr dirFromInput) >>> selectDirection DNothing

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

selectDirection :: Direction -> Wire s e m (Maybe Direction, Square) Direction
selectDirection previousDir = mkSFN $ \(dirFromInput, go) -> 
    let nextInputDir    = fromMaybe previousDir dirFromInput
        forcedNextDir   = shouldForceDirection go
        nextDir         = fromMaybe nextInputDir forcedNextDir
    in  (nextDir, selectDirection nextDir)

shouldForceDirection :: Square -> Maybe Direction
shouldForceDirection go = forcedRight go <|> forcedLeft go <|> forcedUp go <|> forcedDown go

forcedRight :: (HasSize a, HasPosition a) => a -> Maybe Direction
forcedRight obj
    | getX obj - getSize obj / 2 < 0 = Just DRight
    | otherwise = Nothing

forcedLeft :: (HasSize a, HasPosition a) => a -> Maybe Direction
forcedLeft obj
    | getX obj + getSize obj / 2 > windowSize = Just DLeft
    | otherwise = Nothing

forcedUp :: (HasSize a, HasPosition a) => a -> Maybe Direction
forcedUp obj
    | getY obj + getSize obj / 2 > windowSize = Just DUp
    | otherwise = Nothing

forcedDown :: (HasSize a, HasPosition a) => a -> Maybe Direction
forcedDown obj
    | getY obj - getSize obj / 2 < 0 = Just DDown
    | otherwise = Nothing


------------ SDL Graphics ------------
clearScreen :: SDL.Surface -> (Word8, Word8, Word8) -> IO Bool
clearScreen screen (r, g, b) = 
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b >>= 
    SDL.fillRect screen Nothing

drawSquare :: SDL.Surface -> (Word8, Word8, Word8) -> Square -> IO Bool
drawSquare screen (r, g, b) sq =
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b >>=
        let xPos = round . origin . fst . pos $ sq
            yPos = round . origin . snd . pos $ sq
            sz   = round . size   $ sq
        in  SDL.fillRect screen (Just $ SDL.Rect xPos yPos sz sz)
    where
        origin position = position - size sq / 2

------------ SDL Input Events ------------
isKeyPressed :: [SDL.Keysym] -> SDL.SDLKey -> Bool
isKeyPressed xs key = elem key . map SDL.symKey $ xs

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


