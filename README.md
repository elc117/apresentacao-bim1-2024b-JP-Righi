# Ray Casting Game in Haskell

This is a simple ray-casting game built using Haskell and the CodeWorld library. The game is a first-person view where the player can navigate through a map composed of walls of different types, with basic controls for movement and interaction.

## Features

- **Ray Casting**: The game uses ray casting to render a 2D representation of the world in a 3D-like perspective.
- **Customizable Map**: You can modify the map by editing the `testMap` variable.
- **Player Controls**: The player can move forward, backward, and strafe using keyboard keys, as well as rotate the camera view.

## Game Controls

- **Move Forward**: Press `W`
- **Move Backward**: Press `S`
- **Strafe Left**: Press `A`
- **Strafe Right**: Press `D`
- **Look Left/Right**: Move the mouse horizontally to rotate the camera.

## Code Overview

### Vectors

The game uses vector arithmetic to handle player movement and ray casting. Key functions include:

- `normalized`: Normalizes a vector to length 1.
- `angleBetween`: Calculates the angle between two vectors.

### Map

The map is represented by a 2D grid of cells, each with a `WallType` (an integer). The `parseMap` function converts a list of strings into a `Map` type (an array of cells).

```haskell
parseMap :: [String] -> Map
