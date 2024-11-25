module L1 (
    -- * Tipos de dados
    -- ** Básicos
    Posicao, Velocidade, Tempo, Hitbox, Direcao(..), Semente,
    -- ** Mapas
    Mapa(..), Bloco(..), Personagem(..), Entidade(..), Colecionavel(..),
    -- ** Jogo
    Jogo(..), Mouse(..),
    -- * Funções auxiliares fornecidas
    gravidade, geraAleatorios
    ) where