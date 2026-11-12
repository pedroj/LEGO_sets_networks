import sys
from PIL import Image, ImageDraw, ImageFont
def font(sz,bold=False):
    try: return ImageFont.truetype('/System/Library/Fonts/Helvetica.ttc',sz,index=1 if bold else 0)
    except Exception: return ImageFont.load_default()
def comp(sa,sb,out,l2):
    A=Image.open(sa).convert('RGB'); B=Image.open(sb).convert('RGB')
    H=max(A.height,B.height); W=A.width+B.width; HEAD=520
    cv=Image.new('RGB',(W,H+HEAD),'white'); cv.paste(A,(0,HEAD)); cv.paste(B,(A.width,HEAD)); d=ImageDraw.Draw(cv)
    img=Image.open('../../data/models/set_images/starwars_set_images/4485-1.jpg').convert('RGB')
    h=HEAD-80; img=img.resize((int(img.width*h/img.height),h),Image.LANCZOS)
    cv.paste(img,(90,40)); d.rectangle([90,40,90+img.width,40+h],outline=(170,170,170),width=3)
    x=90+img.width+80
    d.text((x,90),"LEGO 4485 — Sebulba's Podracer & Anakin's Podracer",font=font(110,True),fill=(20,20,20))
    d.text((x,260),l2,font=font(52),fill=(70,70,70))
    d.text((x,340),"Edge width = instance-level connections; colour = joint inferred from subcategories.",font=font(52),fill=(70,70,70))
    cv.save(out+'.png',dpi=(300,300)); cv.save(out+'.pdf',resolution=300)
comp('4485_sebulba_nominifig_icons.png','4485_anakin_nominifig_icons.png','4485_network_split_nominifig',"Minifig parts excluded (2 minifig heads 3626b and 2 taps 4599a, all in Sebulba's podracer). Left: Sebulba's, right: Anakin's.")
comp('4485_sebulba_icons.png','4485_anakin_icons.png','with_minifigs/4485_network_split',"Left: Sebulba's Podracer, right: Anakin's Podracer (minifig parts included).")
