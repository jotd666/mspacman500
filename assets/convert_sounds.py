import subprocess,os,struct,pathlib

sox = r"sox.exe"

this_dir = pathlib.Path(__file__).parent.absolute()
wav_files = ["pacman_killed.wav","credit.wav",
"extra_life.wav","bonus_eaten.wav","loop_1.wav","loop_2.wav",
"loop_3.wav","loop_4.wav","loop_eyes.wav",
"ghost_eaten.wav","extra_life.wav",
"loop_fright.wav","eat_1.wav","eat_2.wav","bounce.wav"]
outdir = this_dir / "../sounds"
outdir.mkdir(exist_ok=True)


sampling_rate = 22050
alt_sampling_rate = 16000
nb_duplicates = 2


for wav_file in wav_files:
    print("processing {}...".format(wav_file))
    raw_file = os.path.join(outdir,os.path.splitext(os.path.basename(wav_file))[0]+".raw")
    def get_sox_cmd(sr,output):
        return [sox,"--volume","1.0",wav_file,"--channels","1","-D","--bits","8","-r",str(sr),"--encoding","signed-integer",output]
    used_sampling_rate = alt_sampling_rate if "loop_fright" in wav_file else sampling_rate

    cmd = get_sox_cmd(used_sampling_rate,raw_file)

    subprocess.check_call(cmd)
    with open(raw_file,"rb") as f:
        contents = f.read()
    # pre-pad with 0W, used by ptplayer for idling
    if contents[0] != b'\x00' and contents[1] != b'\x00':
        # add zeroes
        print("padding with zeroes {}...".format(raw_file))
        os.remove(raw_file)
        with open(raw_file,"wb") as f:
           f.write(struct.pack(">H",0))
           f.write(contents)

