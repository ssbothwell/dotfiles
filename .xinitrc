#!/bin/bash

# TODO: Figure out why I have this path export
#export PATH="${PATH}:${HOME}/.local/bin:${HOME}/.xmonad"

# TODO: Replace all of these with Systemd Service
init-keyboard.sh

# TODO: Figure out alternative to `sleep 1`
sleep 1
xcape -e "Hyper_L=Tab;Hyper_R=backslash;Control_L=Escape" &
stalonetray &
dunst &
nm-applet &
xbanish &
xsetroot -solid "#002b36"
xsetroot -cursor_name  left_ptr
exec xmonad
