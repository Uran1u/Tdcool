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
aplicaDano :: Int -> Jogo
aplicaDano = undefined


atacarInimigos :: [Torre] -> [Inimigo] -> [Inimigo]
atacarInimigos torres inimigos = foldr aplicarDano inimigos torres
  where
    aplicarDano torre inimigos' =
      case tipoTorre torre of
        Area -> aplicarDanoArea torre inimigos'  -- Para dano em área, afetamos todos os inimigos dentro do alcance
        SingleTarget -> aplicarDanoSingleTarget torre inimigos'  -- Para single target, afetamos apenas um inimigo
        _ -> inimigos'  -- Em outras situações, não atacamos

calcularDano :: Torre -> Inimigo -> Int
calcularDano torre inimigo =
  if tipoTorre torre == Area
  then dano torre  -- Torre de dano em área aplica dano para todos os inimigos dentro do raio
  else dano torre  -- Para torres single target, o dano é aplicado apenas ao inimigo alvo

aplicarDanoSingleTarget :: Torre -> [Inimigo] -> [Inimigo]
aplicarDanoSingleTarget torre inimigos =
  case closestInimigo of
    Just inimigo -> [inimigo { vidaInimigo = vidaInimigo inimigo - dano torre }] ++ filter (/= inimigo) inimigos
    Nothing -> inimigos  -- Se não houver inimigos dentro do alcance
  where
    closestInimigo = closest inimigos
    closest [] = Nothing
    closest is = Just (minimumBy (comparing (distancia (posicaoTorre torre) . posicaoInimigo)) is)

-- | Adiciona projéteis gerados pelas torres no jogo.
geraProjetil :: Torre -> Tempo -> [Projetil]
geraProjetil = undefined

-- | Atualiza o jogo com base nas ações do jogador (ex.: colocar torres) e no tempo.
atualiza :: Maybe Acao -> Jogo -> Jogo
atualiza = undefined