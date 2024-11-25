{-|
Module      : L3
Description : Movimenta entidades do jogo (inimigos e projéteis)
Copyright   : [Seu Nome] <seu_email@exemplo.com>

Módulo para a realização da L3 do projeto Tower Defense de LI1 em 2023/24.
-}
module L3 where

import Ldata

-- | Atualiza a posição de um inimigo no caminho do mapa.
moverInimigo :: Mapa -> Tempo -> Inimigo -> Inimigo
moverInimigo = undefined

-- | Atualiza a posição de um projétil.
moverProjetil :: Tempo -> Projetil -> Projetil
moverProjetil = undefined

-- | Remove inimigos que chegam ao destino e atualiza a lista restante.
removerInimigosNaBase :: Mapa -> [Inimigo] -> [Inimigo]
removerInimigosNaBase = undefined

-- | Atualiza o estado do jogo movimentando inimigos e projéteis.
movimenta :: Tempo -> Jogo -> Jogo
movimenta = undefined