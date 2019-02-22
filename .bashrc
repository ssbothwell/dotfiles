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
PS1='\W λ '

# VI mode
set -o vi

# Colorize ls
if [ -f ~/.dir_colors ]; then
    eval `dircolors ~/.dir_colors`
fi
alias ls='ls --color=auto'

# PATH
SCRIPTS="${HOME}/.bin"
LOCAL="${HOME}/.local/bin"
GEM="${HOME}/.gem/ruby/2.5.0/bin"
NODE="${HOME}/.node_modules/bin"
XMONAD="${HOME}/.xmonad"

export PATH="${PATH}:${SCRIPTS}:${LOCAL}:${GEM}:${NODE}:${XMONAD}"
#export PATH="${HOME}/.bin:${PATH}:${HOME}/.node_modules/bin:${HOME}/.local/bin:${HOME}/.xmonad"

# Set Vim as default editor (for git etc)
export VISUAL=vim
export EDITOR="$VISUAL"

# Disable XOFF
stty -ixon
source /usr/bin/activate.sh

# Set up Node Version Manager
source /usr/share/nvm/init-nvm.sh

# Xmonad dir defaults
export XMONAD_DATA_DIR="${HOME}/.xmonad"
export XMONAD_CACHE_DIR="${HOME}/.xmonad"
export XMONAD_CONFIG_DIR="${HOME}/.xmonad"

alias mutt=neomutt

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/solomon/.google-cloud-sdk/path.bash.inc' ]; then . '/home/solomon/.google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/solomon/.google-cloud-sdk/completion.bash.inc' ]; then . '/home/solomon/.google-cloud-sdk/completion.bash.inc'; fi
