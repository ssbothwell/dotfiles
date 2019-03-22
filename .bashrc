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

# VI mode
set -o vi

# PATH
SCRIPTS="${HOME}/.scripts"
LOCAL="${HOME}/.local/bin"
GEM="${HOME}/.gem/ruby/2.5.0/bin"
NODE="${HOME}/.node_modules/bin"
YARN="${HOME}/.yarn/bin"
XMONAD="${HOME}/.xmonad"

export PATH="${PATH}:${SCRIPTS}:${LOCAL}:${GEM}:${NODE}:${XMONAD}:${YARN}"

# Set Vim as default editor (for git etc)
export VISUAL=vim
export EDITOR="$VISUAL"

# Disable XOFF
stty -ixon
source /usr/bin/activate.sh

# Set up Node Version Manager
if [ -f /usr/share/nvm/init-nvm.sh ]; then
  source /usr/share/nvm/init-nvm.sh
fi

# Xmonad dir defaults
export XMONAD_DATA_DIR=$XMONAD
export XMONAD_CACHE_DIR=$XMONAD
export XMONAD_CONFIG_DIR=$XMONAD

# Load Gcloud SDK Script
if [ -f ${SCRIPTS}/gcloud ]; then
  source ${SCRIPTS}/gcloud
fi

# Load Aliases/Functions
if [ -f ${SCRIPTS}/aliases ]; then
  source ${SCRIPTS}/aliases
fi

# Set Prompt
PS1='\W λ '
if [ -f ${SCRIPTS}/bash_prompt ]; then
  source ${SCRIPTS}/bash_prompt
fi
