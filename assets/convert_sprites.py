import os,bitplanelib,json
from PIL import Image

sprites_dir = "sprites"

def process_mazes():
    for i in range(1,2):
        maze = Image.open("maze_{}.png".format(i))

        # now the big time saver: detect maze walls & dots from MAME screenshots
        img = Image.new("RGB",(maze.size[0],maze.size[1]-24-16))
        img.paste(maze,(0,-24))
        # pixel 0,5 holds the outline color of the maze wall
        outline_color = img.getpixel((0,5))
        wall_color = img.getpixel((1,5))
        # there's a power dot there: get the color of the dots
        dot_color = img.getpixel((11,19))
        print(outline_color)
        maze_img = Image.new("RGB",(img.size[0],img.size[1]))

        # draw an image with just the maze
        # collect dot positions
        # add 3 rows for virtual coords (ghost targets)
        # add 2 columns for virtual coords (tunnel, keep positive logical coords)
        dot_matrix = [[0]*(img.size[0]//8 + 4) for _ in range(img.size[1]//8 + 3)]
        wall_matrix = [['W']*(img.size[0]//8 + 4) for _ in range(img.size[1]//8 + 3)]

        for y in range(0,img.size[1]):
            ygrid = y//8+3
            dot_row = dot_matrix[ygrid]
            maze_row = wall_matrix[ygrid]
            for x in range(0,img.size[0]):
                xgrid = x//8 + 2
                p = img.getpixel((x,y))
                if p in (wall_color,outline_color):
                    maze_img.putpixel((x,y),p)

                elif p==dot_color:
                    # either power dot or simple dot
                    # check if already determined
                    v = dot_row[xgrid]
                    if not v:
                        p2 = img.getpixel((x+3,y))
                        if p==p2:
                            # big dot
                            dot_row[xgrid]=2
                        else:
                            dot_row[xgrid]=1

        for row in dot_matrix:
            print(",".join(str(x) for x in row))


        # pixel 1,5 holds the fill color of the maze wall
        #maze_img.save("dumps/maze_{}.png".format(i))
        #maze_palette = bitplanelib.palette_extract(maze,0xF0)
        #bitplanelib.palette_image2raw(maze,r"../{}/maze.bin".format(sprites_dir),maze_palette,palette_precision_mask=0xF0)


# palette order matters
# some key colors are located at a 2**n position
# so they can be drawn with just one plane (pacman, white fonts)
game_palette_txt = """
     dc.w	$0000,$022f,$0ff0,$0f00     ; black (0), maze blue (1), pac yellow (2), red (3)
     dc.w   $FBB,$00ff,$04ba,$0d95,$0FFF,$0fbf    ; dot pink (4), whatever, whatever, whatever, white (8), pen gate pink (8+1)
	 dc.w	$0ddf,$0edf,$0fb5,$0fbb,$0F0,$04bf
     ; sprite palette 16-32
     ; red ghost
     dc.w	$0000,$0f00,$022f,$0edf
     ; pink ghost
     dc.w	$0000,$0fbf,$022f,$0edf
     ; cyan ghost
     dc.w	$0000,$00ff,$022f,$0edf
     ; orange ghost
     dc.w	$0000,$0fb5,$022f,$0edf
"""


game_palette = bitplanelib.palette_dcw2palette(game_palette_txt)
bitplanelib.palette_dump(game_palette,r"../src/palette.s",as_copperlist=False)

outdir = "dumps"

def process_tiles():
    json_file = "tiles.json"
    with open(json_file) as f:
        tiles = json.load(f)

    default_width = tiles["width"]
    default_height = tiles["height"]
    default_horizontal = tiles["horizontal"]

    x_offset = tiles["x_offset"]
    y_offset = tiles["y_offset"]

    sprite_page = tiles["source"]

    sprites = Image.open(sprite_page)



    name_dict = {"bonus_{}".format(i):n for i,n in enumerate(["cherry","strawberry","peach","apple","grapes","galaxian","bell","key"])}
    # we first did that to get the palette but we need to control
    # the order of the palette

    #game_palette = bitplanelib.palette_extract(img,0xF0)
    #bitplanelib.palette_dump(game_palette,"palette.s")



    for object in tiles["objects"]:
        if object.get("ignore"):
            continue
        blit_pad = object.get("blit_pad",True)
        name = object["name"]
        start_x = object["start_x"]+x_offset
        start_y = object["start_y"]+y_offset
        horizontal = object.get("horizontal",default_horizontal)
        width = object.get("width",default_width)
        height = object.get("height",default_height)


        nb_frames = object["frames"]
        for i in range(nb_frames):
            if horizontal:
                x = i*width+start_x
                y = start_y
            else:
                x = start_x
                y = i*height+start_y

            area = (x, y, x + width, y + height)
            cropped_img = sprites.crop(area)
            if nb_frames == 1:
                cropped_name = os.path.join(outdir,"{}.png".format(name))
            else:
                cropped_name = os.path.join(outdir,"{}_{}.png".format(name,i))
            cropped_img.save(cropped_name)

            # save
            x_size = cropped_img.size[0]
            sprite_number = object.get("sprite_number")
            sprite_palette = object.get("sprite_palette")
            if sprite_number is not None:
                if x_size != 16:
                    raise Exception("{} (frame #{}) width (as sprite) should 16, found {}".format(name,i,x_size))
                if sprite_palette:
                    sprite_palette = [tuple(x) for x in sprite_palette]
                    bitplanelib.palette_dump(sprite_palette,"../{}/{}.s".format("src",name))
                else:
                    sprite_palette_offset = 16+(sprite_number//2)*4
                    sprite_palette = game_palette[sprite_palette_offset:sprite_palette_offset+4]
                bin_base = "../{}/{}_{}.bin".format(sprites_dir,name,i) if nb_frames != 1 else "../{}/{}.bin".format(sprites_dir,name)
                bitplanelib.palette_image2sprite(cropped_img,bin_base,
                    sprite_palette,palette_precision_mask=0xF0)
            else:
                # blitter object
                if x_size % 16:
                    raise Exception("{} (frame #{}) with should be a multiple of 16, found {}".format(name,i,x_size))
                # pacman is special: 1 plane
                p = bitplanelib.palette_extract(cropped_img,palette_precision_mask=0xF0)
                # add 16 pixelsblit_pad
                img_x = x_size+16 if blit_pad else x_size
                img = Image.new("RGB",(img_x,cropped_img.size[1]))
                img.paste(cropped_img)
                # if 1 plane, pacman frames, save only 1 plane, else save all 4 planes
                used_palette = p if len(p)==2 else game_palette

                namei = "{}_{}".format(name,i) if nb_frames!=1 else name

                bitplanelib.palette_image2raw(img,"../{}/{}.bin".format(sprites_dir,name_dict.get(namei,namei)),used_palette,palette_precision_mask=0xF0)

def process_fonts():
    json_file = "fonts.json"
    with open(json_file) as f:
        tiles = json.load(f)

    default_width = tiles["width"]
    default_height = tiles["height"]
    default_horizontal = tiles["horizontal"]

    x_offset = tiles["x_offset"]
    y_offset = tiles["y_offset"]

    sprite_page = tiles["source"]

    sprites = Image.open(sprite_page)



    name_dict = {"letter_row_0_{}".format(i):chr(ord('A')+i) for i in range(0,16)}
    name_dict.update({"letter_row_1_{}".format(i):chr(ord('P')+i) for i in range(0,11)})
    name_dict["letter_row_1_11"] = "exclamation"
    name_dict.update({"digit_row_0_{}".format(i):chr(ord('0')+i) for i in range(0,10)})
    name_dict["digit_row_0_10"] = "slash"
    name_dict["digit_row_0_11"] = "dash"
    name_dict["digit_row_0_12"] = "quote"
    # we first did that to get the palette but we need to control
    # the order of the palette



    for object in tiles["objects"]:
        if object.get("ignore"):
            continue
        name = object["name"]
        start_x = object["start_x"]+x_offset
        start_y = object["start_y"]+y_offset
        horizontal = object.get("horizontal",default_horizontal)
        width = object.get("width",default_width)
        height = object.get("height",default_height)

        nb_frames = object["frames"]
        for i in range(nb_frames):
            if horizontal:
                x = i*width+start_x
                y = start_y
            else:
                x = start_x
                y = i*height+start_y

            area = (x, y, x + width, y + height)
            cropped_img = sprites.crop(area)
            bn = "{}_{}.png".format(name,i) if nb_frames != 1 else name+".png"
            cropped_name = os.path.join(outdir,bn)
            cropped_img.save(cropped_name)

            # save
            x_size = cropped_img.size[0]

            # blitter object
            if x_size % 8:
                raise Exception("{} (frame #{}) with should be a multiple of 8, found {}".format(name,i,x_size))
            # pacman is special: 1 plane
            p = bitplanelib.palette_extract(cropped_img,palette_precision_mask=0xF0)
            # add 16 pixels if multiple of 16 (bob)
            img_x = x_size+16 if x_size%16==0 else x_size
            img = Image.new("RGB",(img_x,cropped_img.size[1]))
            img.paste(cropped_img)
            # if 1 plane, pacman frames, save only 1 plane, else save all 4 planes
            used_palette = p if len(p)==2 else game_palette

            namei = "{}_{}".format(name,i) if nb_frames != 1 else name
            bitplanelib.palette_image2raw(img,"../{}/{}.bin".format(sprites_dir,name_dict.get(namei,namei)),used_palette,palette_precision_mask=0xF0)

process_mazes()

#process_tiles()

#process_fonts()
