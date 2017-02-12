# ~/.bashrc
#

## ANSI colors
#  
DEFAULT="$(tput setaf 0)"
BLACK="$(tput setaf 0)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
MAGENTA="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
WHITE="$(tput setaf 7)"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


# PROMPT
PS1='\[${MAGENTA}\]\u@\h \[${YELLOW}\]\W \$ '

# Colorize ls
if [ -f ~/.dir_colors ]; then
    eval `dircolors ~/.dir_colors`
fi
alias ls='ls --color=auto'

# PATH additions
[[ -d "${HOME}/.bin" ]] && export PATH="${HOME}/.bin:${PATH}:~/.node_modules/bin"

# 1Password alias
alias 1Password='wine ~/.wine/drive_c/Program\ Files\ \(x86\)/1Password\ 4/1Password.exe'

# Set Vim as default editor (for git etc)
export VISUAL=vim
export EDITOR="$VISUAL"

# DOT Manager
export DOT_REPO="git@github.com:ssbothwell/dotfiles.git"
export DOT_DIR="$HOME/.dotfiles"
fpath=($HOME/.dot $fpath) # <- for completion?
source $HOME/.dot/dot.sh
