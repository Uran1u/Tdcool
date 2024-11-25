{-|
Module      : L1
Description : Verifica colisões no jogo
Copyright   : [Seu Nome] <seu_email@exemplo.com>

Módulo para a realização da L1 do projeto Tower Defense de LI1 em 2023/24.
-}
module L1 where

import Ldata

-- | Verifica se um projétil colidiu com um inimigo.
colisaoProjetilInimigo :: Projetil -> Inimigo -> Bool
colisaoProjetilInimigo = undefined

-- | Verifica se um inimigo alcançou a base.
inimigoNaBase :: Mapa -> Inimigo -> Bool
inimigoNaBase = undefined

-- | Verifica se dois projéteis colidem.
colisaoProjetilProjetil :: Projetil -> Projetil -> Bool
colisaoProjetilProjetil = undefined