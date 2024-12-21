import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.IO
import Data.Maybe (mapMaybe)
import L1
import L2
import L3
import L4
import Ldata

tileSize :: Float
tileSize = 20




initialState :: Mapa -> GameState
initialState mapa = GameState
  { mapa = mapa 
  , playerPosition = (0, 0)
  , playerVelocity = (0, 0)
  , torres = []
  , inimigos = []
  , projeteis []
  , baseVida = 100 
  , playerPosition (0, 0)
  , playerVelocity (0, 0) 
  , mousepos = (0, 0)
  }

main :: IO ()
main = do
    mapa1 <- "maps/map.txt"
    let mapa = parseMap mapa1
    let state = initialState mapa
    
    
    
    
    
    
    
    play
        (InWindow "test1" (800, 600) (100, 100) )
        black
        60
        initialState
        renderGame
        handleInput
        updateGame



