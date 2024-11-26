{-|
Module      : L1
Description : Verifica colisões no jogo
Copyright   : [Seu Nome] <seu_email@exemplo.com>

Módulo para a realização da L1 do projeto Tower Defense de LI1 em 2023/24.
-}
module L1 where

import Ldata

maptest :: Mapa
maptest = Mapa
  { spawnPoint = (0.0, 0.0)
  , endPoint = (5.0, 7.0)
  , grid = [[Ca, Te, Te, Te, Te, Te, Te, Te, Te]
           ,[Ca, Ca, Ca, Ca, Te, Te, Te, Te, Te]
           ,[Te, Te, Te, Ca, Te, Te, Te, Te, Te]
           ,[Te, Te, Te, Ca, Ca, Ca, Ca, Te, Te]
           ,[Te, Te, Te, Te, Te, Te, Ca, Te, Te]
           ]    
  }
-- | Verifica se um projétil colidiu com um inimigo.
colisaoProjetilInimigo :: Projetil -> Inimigo -> Bool
colisaoProjetilInimigo Projetil Inimigo = 
    distancia (posicaoProjetil Projetil) (posicaoInimigo Inimigo) < 0.1

-- | Verifica se um inimigo alcançou a base.
inimigoNaBase :: Mapa -> Inimigo -> Bool
inimigoNaBase mapa Inimigo = 
    distancia (posicaoInimigo Inimigo) (endPoint mapa) < 0.1

-- | Verifica se dois projéteis colidem.
colisaoProjetilProjetil :: Projetil -> Projetil -> Bool
colisaoProjetilProjetil = 
