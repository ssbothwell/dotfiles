#!/bin/bash

export DISPLAY=:0
export XAUTHORITY=/home/solomon/.Xauthority

LAPTOP=eDP-1
MONITOR=DP-2-1


MONITOR_STATE=$(xrandr | grep "$MONITOR connected")

if [[ -n $MONITOR_STATE ]]; then
    echo "extern connected"
    xrandr --output $MONITOR --auto
    xrandr --output $LAPTOP --auto
    xrandr --auto --output $MONITOR --right-of $LAPTOP
else
    echo "extern disconnected"
    xrandr --output $MONITOR --off
    xrandr --output $LAPTOP --auto
fi

(exec /home/solomon/.bin/init-keyboard.sh )
