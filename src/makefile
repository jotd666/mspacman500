#added by python script

PROGNAME = mspacman
HDBASE = K:\jff\AmigaHD
WHDBASE = $(HDBASE)\PROJETS\HDInstall\DONE\WHDLoad
WHDLOADER = ..\$(PROGNAME).slave
SOURCE = $(PROGNAME)HD.s
MAIN = ..\$(PROGNAME)
all: $(MAIN) $(WHDLOADER)

clean:
	del $(MAIN) $(WHDLOADER)
$(WHDLOADER) : $(SOURCE)
	wdate.py> datetime
	vasmm68k_mot -DDATETIME -I$(HDBASE)/amiga39_JFF_OS/include -I$(WHDBASE)\Include -I$(WHDBASE) -devpac -nosym -Fhunkexe -o $(WHDLOADER) $(SOURCE)

$(MAIN) : mspacman.s ptplayer.s maze_data.s
	vasmm68k_mot -phxass -nosym -Fhunkexe -kick1hunks -maxerrors=0 -I$(HDBASE)/amiga39_JFF_OS/include -o $(MAIN) mspacman.s
