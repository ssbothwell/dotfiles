#!/usr/bin/env python
"""
Alsa Audio Volume for xmobar
"""
import alsaaudio


M = alsaaudio.Mixer()
volume = M.getvolume()[0]
ones = round(volume / 10)
zeros = 10 - ones

if volume == 0:
    print(f"   {volume}%")
else:
    print(f" {volume}%")
