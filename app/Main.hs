module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Definição do tipo GameState
data GameState = GameState
    { toolbarpos   :: [(Float, Float)]  -- Posições do toolbar (onde as torres estão inicialmente)
    , dragging     :: Maybe (Float, Float)  -- Posição da torre sendo arrastada (se houver)
    , placedTowers :: [(Float, Float)]  -- Torres já colocadas no mapa
    }

-- Estado inicial do jogo
initialState :: GameState
initialState = GameState 
    { toolbarpos   = [(-350, 250), (-300, 250), (-250, 250)]  -- Posições das torres no toolbar
    , dragging     = Nothing  -- Nenhuma torre está sendo arrastada no início
    , placedTowers = []  -- Nenhuma torre colocada no mapa no início
    }

-- Janela do jogo (tela cheia)
window :: Display 
window = FullScreen  

background :: Color
background = black  -- Cor de fundo da tela

-- Tamanho das torres
towerSize :: Float
towerSize = 30

-- Renderiza o jogo
render :: GameState -> Picture
render state = pictures (renderToolbarItems ++ renderPlacedTowers ++ renderDraggedTower)
    where 
        -- Renderiza as torres no toolbar
        renderToolbarItems = map renderToolbar (toolbarpos state)
        
        -- Renderiza as torres que já foram colocadas no mapa
        renderPlacedTowers = map renderPlacedTower (placedTowers state)
        
        -- Renderiza a torre que está sendo arrastada (se houver)
        renderDraggedTower  = case dragging state of
            Just (x, y) -> [color yellow $ translate x y $ rectangleSolid towerSize towerSize]
            Nothing -> []  -- Se não estiver arrastando nenhuma torre, nada é renderizado

-- Renderiza as torres no toolbar (onde as torres começam)
renderToolbar :: (Float, Float) -> Picture
renderToolbar (x, y) = translate x y (color blue (rectangleSolid towerSize towerSize))

-- Renderiza as torres já colocadas no mapa
renderPlacedTower :: (Float, Float) -> Picture
renderPlacedTower (x, y) = translate x y (color red (rectangleSolid towerSize towerSize))

-- Lida com os eventos do mouse (cliques e movimento)
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
    -- Quando um botão é pressionado, verifica se clicou em alguma torre no toolbar
    let clickedTower = findClickedTower (toolbarpos state) (x, y)
    in case clickedTower of
         Just towerPos -> state { dragging = Just towerPos }  -- Se clicou, começa a arrastar a torre
         Nothing       -> state  -- Se não clicou em nenhuma torre, não faz nada

handleEvent (EventMotion (x, y)) state =
    -- Atualiza a posição da torre enquanto está sendo arrastada
    state { dragging = fmap (const (x, y)) (dragging state) }

handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) state =
    -- Quando o botão é solto, coloca a torre no local onde foi largada
    case dragging state of
        Just towerPos -> 
            -- Depois de soltar a torre, ela fica no lugar onde o mouse foi solto
            state { dragging = Nothing, placedTowers = placedTowers state ++ [(x, y)] }
        Nothing -> state  -- Se não estava arrastando nenhuma torre, não faz nada

handleEvent _ state = state  -- Para outros eventos, não faz nada

-- Função para encontrar a torre clicada (verifica se o mouse está sobre alguma torre do toolbar)
findClickedTower :: [(Float, Float)] -> (Float, Float) -> Maybe (Float, Float)
findClickedTower toolbar (mouseX, mouseY) =
    let isInside (tx, ty) = abs (mouseX - tx) <= towerSize / 2 && abs (mouseY - ty) <= towerSize / 2
    in case filter isInside toolbar of
         (firstTower:_) -> Just firstTower  -- Se clicou em alguma torre, retorna a posição dela
         []             -> Nothing  -- Se não clicou em nenhuma torre, retorna Nothing

-- Função de atualização do jogo (não está fazendo nada no momento)
update :: Float -> GameState -> GameState
update _ state = state  -- Não atualiza nada, o estado do jogo fica igual

-- Função principal que roda o jogo
main :: IO ()
main = play window background 60 initialState render handleEvent update
