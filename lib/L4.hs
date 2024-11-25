{-|
Module      : L4
Description : Atualiza o estado do jogo com base nas ações do jogador e no tempo
Copyright   : [Seu Nome] <seu_email@exemplo.com>

Módulo para a realização da L4 do projeto Tower Defense de LI1 em 2023/24.
-}
module L4 where

import Ldata

-- | Adiciona uma nova torre no mapa em uma posição específica.
colocaTorre :: Posicao -> Jogo -> Jogo
colocaTorre = undefined

-- | Remove inimigos atingidos pelos projéteis e atualiza suas vidas.
aplicaDano :: Jogo -> Jogo
aplicaDano = undefined

-- | Adiciona projéteis gerados pelas torres no jogo.
geraProjetil :: Torre -> Tempo -> [Projetil]
geraProjetil = undefined

-- | Atualiza o jogo com base nas ações do jogador (ex.: colocar torres) e no tempo.
atualiza :: Maybe Acao -> Jogo -> Jogo
atualiza = undefined