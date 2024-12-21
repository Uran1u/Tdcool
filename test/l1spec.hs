module L1 (l1testes) where

import Ldata
import L1 
import Test.HUnit

maptest :: Mapa
maptest = Mapa
  { spawnPoint = (0.0, 0.0)
  , endPoint = (5.0, 7.0)
  , grid = [[S, Te, Te, Te, Te, Te, Te, Te, Te]
           ,[Ca, Ca, Ca, Ca, Te,  Te,Te, Te, Te]
           ,[Te, Te, Te, Ca, Te, Te, Te, Te, Te]
           ,[Te, Te, Te, Ca, Ca, Ca, Ca, Te, Te]
           ,[Te, Te, Te, Te, Te, Te, E, Te, Te]
           ]    
  }
