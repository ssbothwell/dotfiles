#!/bin/bash

## Define a hash of menus
## each item is a space-separated list of executable commands
#declare -A menus
#menus=(['Keyboard']="init-keyboard.sh"
#       ['ToggleMonitor']="dockToggle.sh"
#       ['terminal']="mutt urxvt xterm")
## Set $MENU to be all the keys of the menus hash
#MENU=${!menus[@]}
#
#while true; do
#  CHOICE=$(echo -e $MENU | tr ' ' '\n' | dmenu)
#  if [ -z "${menus[$CHOICE]}" ]; then
#    # No hash key called $CHOICE - must be a command
#    break
#  else
#    # Select the menu chosen from dmenu output
#    MENU=${menus[$CHOICE]}
#  fi
#done
#
#exec $CHOICE
#

declare -A menu
menu=(['foo']="bar"
      ['baz']="bam")

MENU=${!menu[@]}
declare -a array="car dr"
items="${MENU/ /\\n}\n${array/ /\\n}"
echo ${items}
