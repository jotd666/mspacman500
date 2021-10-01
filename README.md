This is a (successful) attempt by jotd to create a 1:1 port of the famous arcade game on Amiga 500 using 100% 68k assembly.

The display is 4:3 so scores, lives, bonuses are on the side rather than on top/botton. The gameplay layout is 1:1 vs
the original, though.

REQUIRES:

- Any kickstart, 512k memory

TODO: 

- mazes: find all colors of each maze for all levels/find the pattern
- intro
- fruit appears: yellow dot on right bottom corner
- 3 other mazes with fruit entry/exit paths
- exit: clear fruit properly
- maze flash when level ends
- fruit bounce
- rip fruit noise (subtract sound loop)
- rip lead instrument from 2nd tune (without bass)


FEATURES:

- original visual & sounds
- original ghost behaviour & speed
- 50 frames per second (PAL) even on a 68000 A500
- all levels & bonuses & intermission sequences
- original intro
- joystick controlled (port 1)
- can run directly from shell or from whdload (fast machines/complex configurations)


    The game has four different colorful mazes that alternate. The orange maze appears in levels 1 and 2, the light blue maze appears in levels 3, 4, and 5, the brown maze appears in levels 6 through 9, and the dark blue maze appears in levels 10 through 13. Starting with level 14, the last two maze configurations alternate between each other every 4th level.
    The fruit bounces around the screen instead of remaining stationary below the ghost pen, starting from one of the tunnel entrances and leaving through another if not eaten.
    The intermissions have been changed to "Acts". The first one shows how Pac-Man and Ms. Pac-Man first meet, the second shows the two chasing each other around the screen, and the third shows Pac-Man and Ms. Pac-Man awaiting the arrival of Junior.
    The orange ghost (originally named "Clyde") is now named "Sue".
    

MINOR ISSUES:

- sound loops not correct (bad loop timing => pops, tricky to make it right)

BUILDING FROM SOURCES:

Prerequesites:

- Windows or Linux
- python
- sox
- vasm 68k

(besides the .bin files created from png by python, the rest of the process could be built on an amiga with phxass
 or some other assembler and sox for the amiga, but you have to be really mad to attempt it in 2021...)

Build process:

- To create the ".bin" files and some palette .s asm files,
  just run the "convert_sprites.py" python script, then use the "convert_sounds.py"
  python script (audio).
- python and sox must be installed to be able to perform the wav2raw conversions

Binary assets must be created first, then makefile must be called to create the "pacman" program


