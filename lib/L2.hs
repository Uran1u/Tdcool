{-|
Module      : L2
Description : Valida estado inicial do jogo
Copyright   : [Seu Nome] <seu_email@exemplo.com>

Módulo para a realização da L2 do projeto Tower Defense de LI1 em 2023/24.
-}
module L2 where

import Ldata

-- | Verifica se o mapa contém um caminho contínuo para os inimigos.
validaMapa :: Mapa -> Bool
validaMapa = undefined

-- | Verifica se as torres estão posicionadas corretamente (não sobre caminhos ou posições inválidas).
validaTorres :: Mapa -> [Torre] -> Bool
validaTorres = undefined

-- | Valida o estado inicial do jogo.
valida :: Jogo -> Bool
valida = undefined