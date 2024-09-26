# Análise e Explicação do Código em Haskell

Este documento apresenta uma análise e explicação detalhada do código fornecido em Haskell. O objetivo é ajudar quem não conhece a linguagem a entender a lógica por trás das funções e como elas se conectam.

---

## Introdução ao Código

O código que estamos analisando foi escrito em Haskell e utiliza uma biblioteca chamada `CodeWorld` para renderizar gráficos interativos. O foco está em criar um jogo simples em que um jogador se move em um mapa, com colisões e renderização por meio de uma técnica chamada "ray casting".

---

## VECTORS (Vetores)

A primeira parte do código lida com operações vetoriais, muito importantes para manipular direções e movimentação do jogador.

### Função `i2d`

Esta função converte um valor inteiro (`Int`) para um valor de ponto flutuante (`Double`), algo comum quando precisamos fazer cálculos mais precisos.

```haskell
i2d :: Int -> Double
i2d = fromIntegral
```
Essa função é útil, pois muitas operações vetoriais exigem valores em Double para evitar perda de precisão. 

### Função normalized

A função normalized normaliza um vetor, ou seja, ajusta o vetor para que ele tenha comprimento 1, sem alterar sua direção.

```haskell
normalized :: Vector -> Vector
normalized v = if len < 1e-6 then (0,0) else scaledVector (1 / len) v
  where
    len = vectorLength v
```
Caso o comprimento seja muito pequeno, o vetor é zerado para evitar problemas de cálculo.

### Função angleBetween

Esta função calcula o ângulo entre dois vetores usando a função atan2 para determinar a direção entre os dois vetores em um plano 2D.

```haskell
angleBetween :: Vector -> Vector -> Double
angleBetween u@(ux, uy) v@(vx, vy) = atan2 det dot
  where
    det = ux * vy - vx * uy
    dot = dotProduct u v
```

---

## MAP (Mapa)

O código trabalha com um mapa simples representado por uma matriz 2D de inteiros. Cada valor inteiro indica um tipo de célula no mapa, como paredes ou caminhos.


### Tipo Map

O tipo Map é definido como uma matriz (array) de inteiros. Cada célula do array contém um valor que representa o tipo da parede ou caminho.

```haskell
type WallType = Int
type Map = A.Array (Int, Int) WallType
```

### Função parseMap

A função parseMap transforma uma lista de strings em um mapa (Map). Cada string representa uma linha do mapa e cada caractere da string é convertido em um número que indica o tipo da célula.

```haskell
parseMap :: [String] -> Map
parseMap rows =
  A.array ((0,0), (w-1,h-1)) (concat (zipWith parseRow [0..] rows))
  where
    w = length (head rows)
    h = length rows
    parseRow j row = zipWith (parseCell j) [0..] row
    parseCell j i cell = ((i,j), read [cell])
```

### Exemplo de Mapa testMap

O mapa de teste testMap é uma matriz que descreve o layout do mundo onde o jogador vai se mover. Nele, 1 representa paredes, 0 é o caminho, e outros números podem representar objetos especiais.

```haskell
testMap ::[String]
testMap =
  [ "111111111111111111111111"
  , "100000100000000010000001"
  , "100000100022000010000001"
  , "100300100022000010000001"
  , "100000100000000010000001"
  , "100000100000000010000001"
  , "100111111000011110000001"
  , "100000000000000000000001"
  , "100000000000000000000001"
  , "100030000003000000030001"
  , "100000000000000000000001"
  , "100000000000000000000001"
  , "111111111111111111111111"
  ]
```

---
# Game State (Estado do Jogo)

O estado do jogo é definido pela posição do jogador, direção, mapa e as teclas pressionadas.

```haskell
data State = State 
  { worldMap :: !Map 
  , playerPos :: !Vector
  , playerDir :: !Vector
  , keysPressed :: !(S.Set T.Text)
  }
  deriving (Show)
```

### Explicação dos Componentes:
- **worldMap**: O mapa onde o jogador está.
- **playerPos**: A posição atual do jogador no mapa.
- **playerDir**: A direção em que o jogador está olhando.
- **keysPressed**: As teclas que estão sendo pressionadas.

---

## Event Handling (Manipulação de Eventos)
O código responde a eventos como teclas pressionadas, movimento do mouse e a passagem do tempo. Cada evento modifica o estado atual do jogo.

### Função handle
Esta função é responsável por processar eventos e atualizar o estado do jogo.

```haskell
handle :: Event -> State -> State
handle e w@(State {..}) = handle' e
  where
    handle' (TimePassing dt) =
      w { playerPos = playerPos `vectorSum` scaledVector (2*dt) speed }
      where
        speed = normalized $
          (keyToDir "W" playerDir)
          `vectorSum` (keyToDir "S" (scaledVector (-1) playerDir))
          `vectorSum` (keyToDir "A" (rotatedVector (pi/2) playerDir))
          `vectorSum` (keyToDir "D" (rotatedVector (-pi/2) playerDir))
        keyToDir k dir =
          if S.member k keysPressed then dir else (0,0)
    handle' (PointerMovement (x, _)) =
      w { playerDir = rotatedVector (-x * pi / 10) (0, 1) }
    handle' (KeyPress k) = w { keysPressed = S.insert k keysPressed }
    handle' (KeyRelease k) = w { keysPressed = S.delete k keysPressed }
    handle' _ = w
```

---

## Ray Casting
O ray casting é a técnica usada para detectar paredes e renderizar o mundo 3D.

### Função cellsVisitedByRay
Esta função calcula quais células no mapa foram "visitadas" por um raio que parte da posição do jogador.

```haskell
cellsVisitedByRay 
  :: Vector -- starting point 
  -> Vector -- direction
  -> [(HitSide, (Int, Int), Double)]
cellsVisitedByRay (posX, posY) (dirX, dirY) =
  (Inside, (initI, initJ), 0) : go initI initJ initTMaxX initTMaxY 
  where
    initI = floor posX
    initJ = floor posY
    stepI = if dirX > 0 then 1 else -1
    stepJ = if dirY > 0 then 1 else -1
    tDeltaX = abs (1 / dirX)
    tDeltaY = abs (1 / dirY)
    xSide = if dirX > 0 then W else E
    ySide = if dirY > 0 then S else N
    initTMaxX =
      if dirX > 0
        then (1 + i2d initI - posX) * tDeltaX
        else (posX - i2d initI) * tDeltaX
    initTMaxY =
      if dirY > 0
        then (1 + i2d initJ - posY) * tDeltaY
        else (posY - i2d initJ) * tDeltaY
    go i j tMaxX tMaxY
      | tMaxX < tMaxY =
          let i' = i + stepI
              tMaxX' = tMaxX + tDeltaX
          in (xSide, (i', j), tMaxX) : go i' j tMaxX' tMaxY
      | otherwise =
          let j' = j + stepJ
              tMaxY' = tMaxY + tDeltaY
          in (ySide, (i, j'), tMaxY) : go i j' tMaxX tMaxY'
```


### Função collision
Esta função detecta a colisão de um raio com uma parede e retorna o lado da parede atingido, o tipo de parede e a distância até o ponto de colisão.

```haskell
collision
  :: Map
  -> Vector -- starting point
  -> Vector -- camera direction
  -> Vector -- ray direction
  -> (HitSide, WallType, Double {- distance -})
collision m pos cameraDir rayDir =
  head 
  $ filter isWall
  $ map convert
  $ cellsVisitedByRay pos rayDir
  where
    convert (side, coord, d) =
      (side, m A.! coord, d * cos (angleBetween cameraDir rayDir))
    isWall (_, wallType, _) = wallType > 0
```

