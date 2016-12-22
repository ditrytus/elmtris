# elmtris

Tetris made in Elm. 

Current version is playable under: (http://ditrytus.github.io/elmtris)

## Compliance with (known) [Tetris Guidlines] (https://tetris.wiki/Tetris_Guideline)

- [x] Playfield is 10 cells wide and at least 22 cells tall, where rows above 20 are hidden or obstructed by the field frame
- [ ] Tetromino colors
- [x] Tetromino start locations
- [x] [Super Rotation System (SRS)] (https://tetris.wiki/SRS) specifies tetromino rotation
- [ ] Standard mappings for computer keyboards
  - [ ] hard drop
  - [ ] non-locking soft drop (left shift, and right shift respectively).
  - [x] rotating both clockwise, and counterclockwise
- [x] So-called Random Generator (also called "random bag" or "7 system")
- [ ] "Hold piece": The player can press a button to send the falling tetromino to the hold box, and any tetromino that had been in the hold box moves to the top of the screen and begins falling. Hold cannot be used again until after the piece locks down.
- [ ] Game must have ghost piece function.
- [ ] Designated soft drop speed.
- [ ] Player may only level up by:
  - [x] clearing lines
  - [ ] performing T-Spin
- [ ] Game must include a song called Korobeiniki.
- [ ] The player tops out when a piece is spawned overlapping at least one block, or a piece locks completely above the visible portion of the playfield.
 
## Other things to do

- [ ] Add sound and music
- [x] Pause
- [x] Display of next-coming tetrominoes.
- [ ] Game should include the songs:
  - [ ] Katjusha
  - [ ] Kalinka.
- [ ] Support for touch gestures, so that a game is playable on mobile devices
- [ ] Display key/gesture mappings dependent on the platform

## Non functional stuff

- [x] Automatically build and publish to github pages with travis CI
- [ ] Give it a nice domain
- [x] Add some front end build pipeline (Just for practice)
