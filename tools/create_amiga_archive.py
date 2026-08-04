import subprocess,os,glob,shutil,pathlib,zipfile

this_dir = pathlib.Path(__file__).parent.absolute()
progdir = this_dir.parent

# packaging tools used:
#
# paraj lha from https://github.com/mras0/plha.git
# exe2adf from https://www.exe2adf.com/
# l-packer from https://www.pouet.net/prod.php?which=105264

gamename = "mspacman"

build_dist = True
clean_dist = True
create_dist = True
create_floppy = True

# JOTD path for cranker, adapt to whatever your path is :)
os.environ["PATH"] += os.pathsep+r"K:\progs\cli"
distdir = progdir / "dist"
outdir = distdir / f"{gamename}_HD"
flopdir = distdir / f"{gamename}_floppy"
assets = progdir /"assets"

cmd_prefix = ["make","-f",progdir / "src" / "makefile"]

if build_dist:
    for s in ["convert_sounds.py"]:
        subprocess.check_call(["cmd","/c",s],cwd=assets)


    subprocess.check_call(cmd_prefix+["clean"],cwd=progdir /"src")

    subprocess.check_call(cmd_prefix+["RELEASE_BUILD=1"],cwd=progdir /"src")
    # create archive


    if os.path.isdir(outdir):
        shutil.rmtree(outdir)
    outdir.mkdir(exist_ok=True,parents=True)

    for file in ["readme.md",f"{gamename}.slave"]:
        shutil.copy(progdir / file,outdir)

    shutil.copy(assets/f"{gamename.title()}.info",outdir)


if create_dist:
    if os.path.isdir(flopdir):
        shutil.rmtree(flopdir)
    flopdir.mkdir(exist_ok=True,parents=True)

    for ext in [""]:
        exename = f"{gamename}{ext}"
        shutil.copy(progdir/exename,outdir)
        print(f"packing {exename}...")
        # cranker creates incorrect exe
        #subprocess.run(["cranker_windows.exe","-m","-t","Vulgus port by JOTD, music by no9","-f",progdir/exename,"-o",progdir/f"{exename}.rnc"],check=True)
        # l-packer creates proper exe, but needs slightly more memory, so add21k is needed
        subprocess.run(["l-packer.exe","-lz4",progdir/exename,flopdir/exename],check=True)

    arcname = progdir / f"{gamename}_HD.lha"
    arcname.unlink(missing_ok=True)
    cmd = ["lha","-r","a",arcname,"*"]

    subprocess.run(cmd,cwd=outdir,check=True)


# create floppy
if create_floppy:

    for ext in [""]:
        exename = flopdir/f"{gamename}{ext}"

        #shutil.copy(assets/"disk.info",flopdir)
        adf_name = progdir/(gamename.title()+".adf")

        xtra_dir = flopdir/"xtra"
        xtra_dir.mkdir(exist_ok=True)
        #shutil.copy(assets/f"disk.info",xtra_dir)

        with (xtra_dir/"floppy").open("w"):
            pass

        shutil.copy(progdir / "readme.md",xtra_dir)

        cmd = ["exe2adf","--add21k","-i",exename,"-a",adf_name,"-l",gamename.title(),"-d",xtra_dir]
        subprocess.run(cmd,cwd=flopdir,check=True)

        # create a .zip for the floppy

        with zipfile.ZipFile(progdir / f"{gamename.title()}_adf.zip",mode="w",compression=zipfile.ZIP_DEFLATED) as zf:
            zf.write(adf_name,arcname=adf_name.name)

if clean_dist:
    subprocess.run(cmd_prefix+["clean"],cwd=progdir/"src",check=True)
