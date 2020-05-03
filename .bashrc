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

# PATH
SCRIPTS="${HOME}/.local/scripts"
LOCAL="${HOME}/.local/bin"
GEM="${HOME}/.gem/ruby/2.5.0/bin"
NODE="${HOME}/.node_modules/bin"
YARN="${HOME}/.yarn/bin"
XMONAD="${HOME}/.xmonad"
CABAL="${HOME}/.cabal/bin"

export PATH="${PATH}:${SCRIPTS}:${LOCAL}:${GEM}:${NODE}:${XMONAD}:${YARN}:${CABAL}"

# Set Vim as default editor (for git etc)
export VISUAL=vim
export EDITOR="$VISUAL"

# Load Aliases
if [ -f ${SCRIPTS}/aliases ]; then
    source ${SCRIPTS}/aliases
fi

if [ -f ${SCRIPTS}/workAliases ]; then
    source ${SCRIPTS}/workAliases
fi

# Load Functions
if [ -f ${SCRIPTS}/functions ]; then
    source ${SCRIPTS}/functions
fi

# Set Prompt
PS1='\W λ '
if [ -f ${SCRIPTS}/bash_prompt ]; then
  source ${SCRIPTS}/bash_prompt
fi
