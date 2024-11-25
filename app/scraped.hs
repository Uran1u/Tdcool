module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Definição do tipo GameState
data GameState = GameState
    { toolbarpos   :: [(Float, Float)]  
    , enemyPos     :: [(Float, Float)]  
    , dragging     :: Maybe (Float, Float)  
    , placedTowers :: [(Float, Float)]  
    , placedEnemy  :: [(Float, Float)]
    }

-- Estado inicial do jogo
initialState :: GameState
initialState = GameState 
    { toolbarpos   = [(-350, 250), (-300, 250), (-250, 250)]  
    , enemyPos     = [(-200, 250)]  
    , dragging     = Nothing  
    , placedTowers = []  
    , placedEnemy  = []
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
render state = pictures (renderToolbarItems ++ renderEnemies ++ renderPlacedTowers ++ renderPlacedEnemies ++ renderDraggedTower)
    where 
        -- Renderiza as torres no toolbar
        renderToolbarItems = map renderToolbar (toolbarpos state)
        
        -- Renderiza os inimigos no toolbar
        renderEnemies = map renderEnemy (enemyPos state)
        
        -- Renderiza as torres que já foram colocadas no mapa
        renderPlacedTowers = map renderPlacedTower (placedTowers state)

        renderPlacedEnemies = map renderPlacedEnemy (placedEnemy state)
        
        -- Renderiza a torre que está sendo arrastada (se houver)
        renderDraggedTower  = case dragging state of
            Just (x, y) -> [color yellow (translate x y (rectangleSolid towerSize towerSize))]
            Nothing -> []  -- Se não estiver arrastando nenhuma torre, nada é renderizado

-- Renderiza as torres no toolbar (onde as torres começam)
renderToolbar :: (Float, Float) -> Picture
renderToolbar (x, y) = translate x y (color blue (rectangleSolid towerSize towerSize))

-- Renderiza os inimigos no toolbar
renderEnemy :: (Float, Float) -> Picture
renderEnemy (x, y) = translate x y (color red (circleSolid (towerSize / 2)))

-- Renderiza as torres já colocadas no mapa
renderPlacedTower :: (Float, Float) -> Picture
renderPlacedTower (x, y) = translate x y (color red (rectangleSolid towerSize towerSize))

renderPlacedEnemy:: (Float, Float) -> Picture
renderPlacedEnemy (x,y) = translate x y (color red (circleSolid (towerSize / 2)))

-- Lida com os eventos do mouse (cliques e movimento)
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
    -- Quando um botão é pressionado, verifica se clicou em alguma torre ou inimigo no toolbar
    let clickedTower = findClickedTower (toolbarpos state) (x, y)
        clickedEnemy = findClickedTower (enemyPos state) (x, y)  -- Reutilizando a lógica para inimigos
    in case (clickedTower, clickedEnemy) of
         (Just towerPos, _) -> state { dragging = Just towerPos }  -- Se clicou em uma torre, começa a arrastar
         (_, Just enemyPos) -> state { dragging = Just enemyPos }  -- Se clicou em um inimigo, começa a arrastar
         _                  -> state  -- Se não clicou em nada, não faz nada

handleEvent (EventMotion (x, y)) state =
    -- Atualiza a posição da torre enquanto está sendo arrastada
    state { dragging = fmap (const (x, y)) (dragging state) }

handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) state =
    -- Quando o botão é solto, coloca a torre ou inimigo no local onde foi largado
    case dragging state of
        Just (dx, dy)
            |(dx, dy)`elem` toolbarpos state ->
        
            -- Depois de soltar a torre, ela fica no lugar onde o mouse foi solto
            state { dragging = Nothing, placedTowers = placedTowers state ++ [(x, y)] }
            |(dx, dy)`elem` enemyPos state ->
            -- Soltar um inimigo: adiciona à lista de inimigos colocados
            state { dragging = Nothing, placedEnemy = placedEnemy state ++ [(x, y)] }

        Nothing -> state  -- Se não estava arrastando nada, não faz nada

handleEvent _ state = state  -- Para outros eventos, não faz nada

-- Função para encontrar a torre clicada (ou inimigo)
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



-- Function to render the game state
render :: GameState -> Picture
render state = pictures (renderToolbar ++ renderEnemies ++ renderTowers)
  where
  
    renderToolbar = map renderToolbarItem (toolbarPos state)

    renderEnemies = map renderEnemy (enemyPos state)

    renderTowers = map renderTower (pdtower state)

    -- Render a single toolbar item (e.g., draggable icons)
    renderToolbarItem :: (Float, Float) -> Picture
    renderToolbarItem (x, y) = translate x y (color blue (rectangleSolid 30 30))

    -- Render a single enemy
    renderEnemy :: (Float, Float) -> Picture
    renderEnemy (x, y) = translate x y (color red (circleSolid 15))  -- Example enemy as a red circle

    -- Render a single tower
    renderTower :: Tower -> Picture
    renderTower (Tower { towerPos = (x, y), towerHealth = health }) =
        translate x y (pictures [color yellow (rectangleSolid 30 30), renderHealthBar health])

    -- Render a health bar above a tower
    renderHealthBar :: Float -> Picture
    renderHealthBar health =
        translate 0 20 (color green (rectangleSolid (health / 10) 5))  -- Example health bar
