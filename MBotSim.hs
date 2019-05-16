module MBotSim
( openMBot
, closeMBot
, setRGB
, setMotor
, readUltraSonic
, readLineFollower
, sendCommand
, runSim , startwereld, makeWorld
, Command (..)
, Line (..)
, Device (..)
) where

import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)

data Command = RGBCommand Int Int Int Int | MotorCommand Int Int deriving(Show, Eq)

data Line = LEFTB | RIGHTB | BOTHB | BOTHW

type RGB = (Int, Int, Int)

-- Device stelt de hele gesimuleerde wereld voor en wordt in een MVar gewrapt zodat de gloss functies en de sendCommand functies op elkaar moeten wachten vanuit verschillende threads
data Device = Device (MVar World)
data World = World { bot :: G.Point
             , motors :: (Int, Int)
             , leds :: (RGB, RGB)
             , dir :: Float
             , walls :: [G.Point]
             , wLines :: [G.Point] -- aparte blokken waar lijnen staan
             , lineTups :: [(G.Point, G.Point)] -- tupels met begin en eindpunten van lijnen
} deriving(Eq, Ord)

instance Show World where
  show w = "Pos MBot: " ++ (show (bot w)) ++ "\nMotors: " ++ (show (motors w)) ++ "\nLeds: " ++ (show (leds w)) ++ "\nDirection: " ++ (show (dir w)) ++ "\nWalls: " ++ (show (walls w)) ++ "\nLines spanning (from, to): " ++ (show (lineTups w))

emptyWorld :: World
emptyWorld = World (0.0,0.0) (3,1) ((255,0,0),(0,0,255)) 0.0 [] [] []

startwereld :: [String]
startwereld = ["+------------------+", "|                  |", "|  X    >          |","|  X               |", "|  XXXXXXXXXX      |", "|           X      |", "|           X      |", "+------------------+", "(0,0)     (10,10)", "(10,10)  (20,10)"]

-------------------------------------------------------------------------------------------------------------------------------------------------
-- hulpfuncties voor de module
-- een modulo functie voor kommagetallen
fmod :: Float -> Float -> Float
fmod a b
   | b <= 0.0 = 0.0
   | a > 0 && a >= b = fmod (a-b) b
   | a < 0 && (abs a) >= b = fmod (a+b) b
   | otherwise = a

-- bepaal de maximum Y-waarde uit een lijst van G.Pointinaten
maxTup :: [G.Point] -> Float
maxTup = maximum . (map snd)

-- Maak van een string "(a,b)WHITESPACE(c,d)" een tuple met 2 G.Pointinaten (G.Point,G.Point
makeTupOfString :: String -> (G.Point, G.Point)
makeTupOfString str@(x:xs) = (co1, co2)
                            where co1 = read (takeWhile (\x->x/=' ' && x/='\t') str) :: G.Point
                                  co2 = read (dropWhile (\x->x/='(') xs) :: G.Point

moveBot :: Float -> World -> World
moveBot deltaT (World (x,y) (vl, vr) leds dir walls liness ll) = World (x', y') (vl, vr) leds dir' walls liness ll
                                                                  where r = 1 -- straal wiel (arbitrair)
                                                                        a = 2 -- afstand tss wielen (arbitrair)
                                                                        deltaX = deltaT * r / 2 * (fromIntegral (vl + vr)) * (cos dir)
                                                                        deltaY = deltaT * r / 2 * (fromIntegral (vl + vr)) * (sin dir)
                                                                        deltaD = deltaT * r / a * (fromIntegral (vr - vl))
                                                                        x' = x + deltaX
                                                                        y' = y + deltaY
                                                                        dir' = fmod (dir + deltaD) (2*pi)


-- Parsing
withCoords :: [String] -> [(Char, G.Point)]
withCoords level = [ (chr, (c, r))
                  | (r, row) <- zip [0..] level
                  , (c, chr) <- zip [0..] row
                  ]

-- aangezien er een functie is die mooi alle G.Pointinaten samen met de karakters in het bestand teruggeeft
-- is het aanmaken van een wereld nu gewoon te implementeren via een fold op een lege wereld voor de muren en robot
-- het inlezen van de lijnG.Pointinaten gebeurt via een hulpfunctie die gemapt wordt over de regels waar de G.Pointinaten in staan
makeWorld :: [String] -> World
makeWorld lvl = (foldr (uncurry add) emptyWorld (withCoords lvl)) {lineTups = (map makeTupOfString lijntjens)}
 where add '>' co (World bot motors leds dir walls wLines lineTups) = World co motors leds 0.0 walls wLines lineTups
       add '<' co (World bot motors leds dir walls wLines lineTups) = World co motors leds pi walls wLines lineTups
       add '^' co (World bot motors leds dir walls wLines lineTups) = World co motors leds (pi/2.0) walls wLines lineTups
       add 'v' co (World bot motors leds dir walls wLines lineTups) = World co motors leds (3.0*pi/2.0) walls wLines lineTups
       add '-' co (World bot motors leds dir walls wLines lineTups) = World bot motors leds dir (co : walls) wLines lineTups
       add '|' co (World bot motors leds dir walls wLines lineTups) = World bot motors leds dir (co : walls) wLines lineTups
       add 'X' co (World bot motors leds dir walls wLines lineTups) = World bot motors leds dir (co : walls) wLines lineTups
       add '+' co (World bot motors leds dir walls wLines lineTups) = World bot motors leds dir (co : walls) wLines lineTups
       add  _  co wld = wld
       lijntjens = [ str | str@(x:xs) <- lvl , x == '(']

mirrorY :: World -> World
mirrorY (World pos motors leds dir w l ll) = World (spiegel pos) motors leds dir (map spiegel w) (map spiegel l) ll
                          where ymax = maxTup w
                                spiegel = (\(x,y)->(x,ymax-y))

doCmd :: World -> Command -> World
doCmd (World pos motors (_, rgb2) dir w l ll) (RGBCommand 1 r g b) = World pos motors ((r,g,b), rgb2) dir w l ll
doCmd (World pos motors (rgb1, _) dir w l ll) (RGBCommand 2 r g b) = World pos motors (rgb1, (r,g,b)) dir w l ll
doCmd wrld (MotorCommand l r) = wrld {motors = (l,r)}

-------------------------------------------------------------------------------------------------------------------------------------------------
-- gloss functies
render :: G.Picture -> G.Picture
      -> World -> G.Picture
render botpic wallpic (World bot motors leds dir walls lns ll) = G.pictures $
                 [renderPicAt botpic bot]
          ++ map (renderPicAt wallpic) walls
          ++ map renderLine ll
 where cell = 32.0
       size which = maximum $ map which walls
       toPix which = (+ (cell / 2 - cell * size which / 2))
                   . (* cell)
       renderPicAt picture (x, y) = G.translate (toPix fst x)
                                                (toPix snd y)
                                                picture
       renderLine ((x1,y1), (x2,y2)) = G.line [(toPix fst x1, toPix snd y1), (toPix fst x2, toPix snd y2)]


renderFromMVar :: G.Picture -> G.Picture -> Device -> IO G.Picture
renderFromMVar botpic wallpic (Device m) = do
  wrld <- takeMVar m
  putMVar m wrld
  return $ render botpic wallpic (mirrorY wrld)

-- behandelt het verloop van de tijd, m.a.w. update de positie van de robot indien zijn motorsnelheden /= 0 zijn
passTime :: Float -> Device -> IO Device
passTime deltaT (Device m) = do
  wrld <- takeMVar m
  putMVar m $ moveBot deltaT wrld
  putStrLn $ "moved to " ++ (show (bot (moveBot deltaT wrld)))
  return $ Device m

-- unsafePassTime :: Float -> Device -> Device
-- unsafePassTime deltaT dev = unsafePerformIO $ passTime deltaT dev

-- G.Play :: Display                            -- Het scherm
--           -> Color                           -- De achtergrondkleur
--           -> Int                             -- # fps dat berekend moet worden
--           -> world                           -- initiële wereld (ingeladen uit tekst) (zal van het type IO Device zijn (MVar wrapper voor communicatie, IO om mvar operaties te kunnen chainen))
--           -> (world -> IO Picture)           -- de render functie ondersteunt IO om MVar functions te chainen
--           -> (Event -> world -> IO world)       -- event handler - niet nodig
--           -> (Float -> world -> IO world)       -- time handling functie, deze functie zal reageren op het tijdsverloop (verplaatsing + aanpassing hoek van de robot afhankelijk van zijn motorsnelheden)
--           -> IO ()
runSim :: Device -> IO ()
runSim wrld = do
  [rpic, wpic] <- mapM loadBMP ["robot.bmp", "walli.bmp"]
  G.playIO (G.InWindow "MBot simulatie" (800,800) (0,0))
         G.white
         30
         wrld -- world :: Device
         (\wrld -> renderFromMVar rpic wpic wrld)
         (\ev wrld -> return wrld)
         passTime
-------------------------------------------------------------------------------------------------------------------------------------------------
-- Library functies die exported worden

-- Open de connectie met de MBot uit de meegegeven wereld en geef een Device terug
openMBot :: IO Device
openMBot = do
  container <- newEmptyMVar
  putMVar container (makeWorld startwereld)
  -- start simulatie in aparte thread
  putStrLn "now forking thread to spawn simulation"
  forkIO $ runSim $ Device container
  return $ Device container

-- Sluit de connectie met de MBot
closeMBot :: Device -> IO ()
closeMBot (Device m) = do
  wrld <- takeMVar m
  putStrLn "now disconnecting from device..."
  putStrLn $ "World state was: " ++ (show wrld)

-- Maakt een commando om de led aan te schakelen.
-- Index (1,2) geeft aan welke led je wilt aanschakelen.
setRGB :: Int -> Int -> Int -> Int -> Command
setRGB i r g b = RGBCommand i r g b

-- Gegeven de snelheid van het linkerwiel en rechterwiel (-255 tot 255)
-- maak een commando om de motors te laten ronddraaien
setMotor :: Int -> Int -> Command
setMotor vl vr = MotorCommand vl vr

-- Lees de ultrasone sensor uit
readUltraSonic :: Device -> IO Float
readUltraSonic (Device m) = undefined

-- Lees de line follow sensor uit. Dit geeft een Line terug
readLineFollower :: Device -> IO Line
readLineFollower (Device m) = undefined

-- Verstuur een commando naar de MBot
sendCommand :: Device -> Command -> IO ()
sendCommand (Device m) cmd = do
  wrld <- takeMVar m
  putStrLn $ "wereld voor command :" ++ show wrld
  putStrLn $ "command: " ++ show cmd
  putMVar m (doCmd wrld cmd)
