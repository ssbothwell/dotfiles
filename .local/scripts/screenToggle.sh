#!/bin/bash
# Xrandr monitor activation script. 

ACTIVEOUTPUT=$(xrandr | grep -E " connected (primary )?[1-9]+" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/")
LAPTOP=eDP-1
EXTERNAL=HDMI-2
CHOICE=$1

case "$CHOICE" in
  laptop) CHOICE=$LAPTOP ;;
  external) CHOICE=$EXTERNAL ;;
  *) echo "No such screen" && break ;;
esac

if grep -q $CHOICE <<<$ACTIVEOUTPUT; then 
  echo "${CHOICE} is enabled"
  xrandr --output $CHOICE --off
else
  echo "${CHOICE} is disabled"
  xrandr --output $CHOICE --auto
  if [ $CHOICE == $MONITOR ]; then
    xrandr --auto --output $MONITOR --right-of $LAPTOP
  fi
fi
