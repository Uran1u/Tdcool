module Ldata (
    -- Tipos básicos
    Posicao, Velocidade, Tempo,
    -- Tipos do jogo
    Torre(..), TipoTorre(..), Inimigo(..), Projetil(..), Mapa(..), GameState(..),Bloco(..),
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

data TipoTorre = Fogo | Gelo | Relampago | Area | SingleTarget
  deriving (Eq, Show)


data Torre =
  Torre
    { posicaoTorre  :: Posicao
    , tipoTorre     :: TipoTorre  -- Tipo da torre
    , alcance       :: Double
    , dano          :: Int
    , recarga       :: Tempo -- Tempo entre disparos
    }
  deriving (Eq, Read, Show)


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
    { spawnPoint  :: Posicao        -- ^ Posição inicial dos inimigos
    , endPoint    :: Posicao        -- ^ Posição final que os inimigos devem alcançar
    , grid        :: [[Bloco]]      -- ^ Representação do mapa em uma matriz de blocos
    }
  deriving (Eq, Read, Show)

  -- | Tipos de blocos no mapa.
data Bloco
  = C      -- ^ Caminho por onde os inimigos se movem
  | T         -- ^ Posição onde pode ser construída uma torre
  | O      -- ^ Obstáculo que impede o movimento
  | V         -- ^ Espaço vazio
  | S
  | E
  deriving (Eq, Read, Show)

-- | Representa o estado do jogo.
data GameState =
  GameState
    { mapa      :: Mapa
    , torres    :: [Torre]
    , inimigos  :: [Inimigo]
    , projeteis :: [Projetil]
    , baseVida  :: Int -- Vida restante da base
    , playerPosition :: (Float, Float)
    , playerVelocity :: (Float, Float)
    , mousepos :: (Float, Float)
    , 
    }
  deriving (Eq, Read, Show)

-- | Calcula a distância entre duas posições.
distancia :: Posicao -> Posicao -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- | Vetor velocidade da gravidade (para projéteis, se necessário).
gravidade :: Velocidade
gravidade = (0, 10)