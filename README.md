MsPacman 500

This is a (successful) attempt by jotd to create a 1:1 port of the famous arcade game on Amiga 500 using 100% 68k assembly.

The display is 4:3 so scores, lives, bonuses are on the side rather than on top/botton. The gameplay layout is 1:1 vs
the original, though.

REQUIRES:

- Kickstart 1.3, 512k memory or
- Kickstart 2.0, 1MB memory

FEATURES:

- original visual & sounds
- original ghost behaviour & speed
- 50 frames per second (PAL) even on a 68000 A500
- all levels & bonuses & intermission sequences
- original intro
- joystick controlled (port 1)
- can run directly from shell or from whdload (fast machines/complex configurations)

ISSUES: 

- eat fruit: sometimes stuff remaining
- ms pac sometimes pixels remaining (when eating ghosts?)
- redo demo mode

MINOR ISSUES:

- quitting game when player is killed crashes on kickstart 1.3 (test with whdload kick1.3)
- ghosts in pen bounce too fast in high levels

ABOUT ACTS:

 The intermissions have been changed to "Acts". The first one shows how Pac-Man and Ms. Pac-Man first meet,
 the second shows the two chasing each other around the screen,
 and the third shows Pac-Man and Ms. Pac-Man awaiting the arrival of Junior.
  
CREDITS:

- jotd: code and gfx/sfx conversion
- no9: music conversion to protracker
- phx: sfx/module player
- meynaf: random routine
- eab forum: useful advice & support
- Rob Northen: unpacker (http://aminet.net/util/pack/RNC_ProPack.lha)
- 125scratch: sprite rips https://www.spriters-resource.com/arcade/mspacman/
- midway/gcc: original game :)

BUILDING FROM SOURCES:

Prerequesites:

- Windows
- python
- Amiga NDK
- sox (included)
- vasm 68k (included)
- rnc (Rob Nothen cruncher unofficial port, included)

* besides the .bin files created from png by python, the rest of the process could be built on an amiga with phxass
 or some other assembler and sox for the amiga, but you have to be really mad to attempt it in 2021...)
* could be done on Linux, just rebuild the rnc cruncher & vasm

Build process:

- To create the ".bin" files and some palette .s asm files, from "assets" subdir, 
  just run the "convert_sprites.py" python script, then use the "convert_sounds.py"
  python script (audio).
- python and sox must be installed to be able to perform the wav2raw conversions
- get "bitplanelib.py" (asset conversion tool needs it) at https://github.com/jotd666/amiga68ktools.git

Binary assets must be created first, then makefile must be called to create the "mspacman" program


