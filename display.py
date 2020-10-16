#!/usr/bin/python
# -*- coding:utf-8 -*-
import sys
import os
import time
libdir="/home/pi/e-Paper/RaspberryPi&JetsonNano/python/lib"
if os.path.exists(libdir):
    sys.path.append(libdir)

from waveshare_epd import epd7in5_V2
from PIL import Image,ImageDraw,ImageFont

try:
    epd = epd7in5_V2.EPD()
    epd.init()
#    epd.Clear()
    Himage = Image.open("screen-output.bmp")
    epd.display(epd.getbuffer(Himage))
    #time.sleep(5)
    epd.sleep()
#    epd.Dev_exit()
except KeyboardInterrupt:    
    epd7in5.epdconfig.module_exit()
    exit()
