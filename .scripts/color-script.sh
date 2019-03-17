#!/bin/bash

## ANSI colors
DEFAULT="$(tput setaf 0)"
BLACK="$(tput setaf 0)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
MAGENTA="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
WHITE="$(tput setaf 7)"

BOLD="$(tput bold)"
NORMAL="$(tput sgr0)"

# PROMPT
#export PS1='\[${YELLOW}\]\W λ '

# BACKGROUND
echo -e '\033]11;#fdf6e3\007'
# TEXT
echo -e '\033]10;#657b83\007'
# CURSOR
echo -e '\033]12;#93a1a1\007'
