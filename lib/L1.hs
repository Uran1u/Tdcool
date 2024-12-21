{-|
Module      : L1
Description : Verifica colisões no jogo e Mapas
Copyright   : [Seu Nome] <seu_email@exemplo.com>

Módulo para a realização da L1 do projeto Tower Defense de LI1 em 2024/25.
-}
module L1 where
import Data.Maybe (mapMaybe)

import Ldata


parseMap :: String -> Mapa 
parseMap input = Mapa 
  { spawnPoint = findPoint 'S' grid 
  , endPoint = findPoint 'E' grid 
  , grid = parseGrid (lines input)
  }
findPoint ::Char -> [[Bloco]]
findPoint point grid = 
  let (row, col) = Head [(r, c) |(r, row) <- zip [0..] ,(c,bloco)<- zip [0..] row, bloco == parseBloco point]
  in (fromIntegral col, fromIntegral row)


parsegrid :: [string] -> [[Bloco]]
parsegrid = map (map parseBloco)

parseBloco :: Char -> Bloco
parseBloco 'C'= C
parseBloco 'T'= T
parseBloco 'O'= O 
parseBloco 'V'= V 
parseBloco 'S'= S 
parseBloco 'E'= E 
parseBloco _ = error "invalide title"

colisoesObstaculo

-- | Verifica se um projétil colidiu com um inimigo.
colisaoProjetilInimigo :: Projetil -> Inimigo -> Bool
colisaoProjetilInimigo projetil inimigo =
    distancia (posicaoProjetil projetil) (posicaoInimigo inimigo) < 0.1

-- | Verifica se um inimigo alcançou a base.
inimigoNaBase :: Mapa -> Inimigo -> Bool
inimigoNaBase mapa inimigo =
    distancia (posicaoInimigo inimigo) (endPoint mapa) < 0.1






