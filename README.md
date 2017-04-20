# Haskell 2d SDL Game Engine

This is an attempt at defining a pure, functional Haskell based SDL game engine with 2D tiles, forces, collision detection and scriptable config files for defining AI, Entities, tilesets, levels and UI functionality.

'GameEngine/Game.hs' shows an example game.

## Directory Structure

| Filepath                                                                     | Contains                                                                                                                           |
| ---------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| [GameEngine.AI.Agent](/GameEngine/AI/Agent.hs)                             | An agent is a thing with some internal state that is passed observations which update its state and decide some action to perform. |
| [GameEngine.AI.Client](/GameEngine/AI/Client.hs)                           | Clients are things that can be updated by an action upon them, or observed for their current state.                                |
| [GameEngine.AI.Live](/GameEngine/AI/Live.hs)                               | A Live thing is a Client and an Agent combined such that it can be updated with an input.                                          |
| [GameEngine.AI.Reproducing](/GameEngine/AI/Reproducing.hs)                 | A Live thing which may create more of itself by explicitly creating a list of more of similarly typed Live objects when updated.   |
| [GameEngine.ConfigReader](/GameEngine/ConfigReader.hs)                     | Read a game config file.                                                                                                           |
| [GameEngine.ConfigReader.Arg](/GameEngine/ConfigReader/Arg.hs)             | Arguments in the game config file are one of several enumerated types.                                                             |
| [GameEngine.ConfigReader.ArgFmt](/GameEngine/ConfigReader/ArgFmt.hs)       | Require an argument of the format specified in the similarly nested 'Arg' module.                                                  |
| [GameEngine.ConfigReader.Config](/GameEngine/ConfigReader/Config.hs)       | Read and decide game configuration.                                                                                                |
| [GameEngine.ConfigReader.Modifier](/GameEngine/ConfigReader/Modifier.hs)   | Word modifiers found in config files.                                                                                              |
| [GameEngine.ConfigReader.Option](/GameEngine/ConfigReader/Option.hs')      | Config options are words and args.                                                                                                 |
| [GameEngine.ConfigReader.Parser](/GameEngine/ConfigReader/Parser.hs)       | Parse config files.                                                                                                                |
| [GameEngine.Stage](/GameEngine/Stage.hs)                                   | A stage holds a collection of objects, backgrounds, etc.                                                                           |
| [GameEngine.Thing](/GameEngine/Thing.hs)                                   | Things have a tile and several physical properties.                                                                                |
| [GameEngine.Tile](/GameEngine/Tile.hs)                                     | A 2D tile.                                                                                                                         |
| [GameEngine.TileGrid](/GameEngine/TileGrid.hs)                             | A 2D grid of instances of a tileset defines a tilegrid.                                                                            |
| [GameEngine.TileSet](/GameEngine/TileSet.hs)                               | A mapping of names to tiles.                                                                                                       |
| [GameEngine.UI/Text](/GameEngine/UI/Text.hs)                               | Textual UI elements.                                                                                                               |
| [GameEngine.Background](/GameEngine/Background.hs)                         | A Background is a Tilegrid plus a possible background texture.                                                                     |
| [GameEngine.Camera](/GameEngine/Camera.hs)                                 | A Camera has positions and dimensions and may track a subject.                                                                     |
| [GameEngine.Collect](/GameEngine/Collect.hs)                               | Collectable things.                                                                                                                |
| [GameEngine.Counter](/GameEngine/Counter.hs)                               | A Counter keeps a number between a counted range.                                                                                  |
| [GameEngine.Force](/GameEngine/Force.hs)                                   | Forces affect velocities.                                                                                                          |
| [GameEngine.Game](/GameEngine/Game.hs)                                     | An example game using the GameEngine.                                                                                              |
| [GameEngine.HitBox](/GameEngine/HitBox.hs)                                 | A Rectange that may be collided with.                                                                                              |
| [GameEngine.Rectangle](/GameEngine/Rectangle.hs)                           | A rectangular shape.                                                                                                               |

