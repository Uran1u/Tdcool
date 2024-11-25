module Ldata (
    -- Tipos básicos
    Posicao, Velocidade, Tempo,
    -- Tipos do jogo
    Torre(..), Inimigo(..), Projetil(..), Mapa(..), Jogo(..),
    -- Funções auxiliares
    distancia, gravidade
) where

import System.Random (mkStdGen, randoms)

-- | Posição no mapa.
type Posicao = (Double, Double)

-- | Vetor velocidade.
type Velocidade = (Double, Double)

-- | Períodos de tempo.
type Tempo = Double

-- | Representa uma torre no jogo.
data Torre =
  Torre
    { posicaoTorre  :: Posicao
    , alcance       :: Double
    , dano          :: Int
    , recarga       :: Tempo -- Tempo entre disparos
    }
  deriving (Eq, Read, Show)

-- | Representa um inimigo no jogo.
data Inimigo =
  Inimigo
    { posicaoInimigo :: Posicao
    , velocidade     :: Velocidade
    , vidaInimigo    :: Int
    }
  deriving (Eq, Read, Show)

-- | Representa um projétil disparado por uma torre.
data Projetil =
  Projetil
    { posicaoProjetil :: Posicao
    , alvo            :: Posicao -- Posição do inimigo alvo
    , velocidadeProj  :: Velocidade
    , danoProjetil    :: Int
    }
  deriving (Eq, Read, Show)

-- | Representa o mapa do jogo.
data Mapa =
  Mapa
    { caminho :: [Posicao] -- Lista de posições que formam o caminho dos inimigos
    }
  deriving (Eq, Read, Show)

-- | Representa o estado do jogo.
data Jogo =
  Jogo
    { mapa      :: Mapa
    , torres    :: [Torre]
    , inimigos  :: [Inimigo]
    , projeteis :: [Projetil]
    , baseVida  :: Int -- Vida restante da base
    }
  deriving (Eq, Read, Show)

-- | Calcula a distância entre duas posições.
distancia :: Posicao -> Posicao -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- | Vetor velocidade da gravidade (para projéteis, se necessário).
gravidade :: Velocidade
gravidade = (0, 10)