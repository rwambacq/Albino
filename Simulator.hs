import MBotSim
import Control.Concurrent

main = do
  d <- openMBot
  sendCommand d $ setRGB 1 0 0 100
  sendCommand d $ setRGB 2 100 0 0
  closeMBot d
