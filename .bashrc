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

# Colorize ls
eval `dircolors`
alias ls='ls --color=auto'

# PATH
SCRIPTS="${HOME}/.bin"
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
source /usr/share/nvm/init-nvm.sh

# Xmonad dir defaults
export XMONAD_DATA_DIR=$XMONAD
export XMONAD_CACHE_DIR=$XMONAD
export XMONAD_CONFIG_DIR=$XMONAD

# Update PATH for the Google Cloud SDK.
if [ -f "${HOME}/.google-cloud-sdk/path.bash.inc" ]; then
  . "${HOME}/.google-cloud-sdk/path.bash.inc";
fi

# Shell command completion for gcloud.
if [ -f "${HOME}/.google-cloud-sdk/completion.bash.inc" ]; then
  . "${HOME}/.google-cloud-sdk/completion.bash.inc";
fi

# common path alias'
alias tripp="cd ${HOME}/Development/trippinc/firebase-backend"
alias trippfunc="cd ${HOME}/Development/trippinc/firebase-backend/Functions/src/GraphQL"
alias mutt=neomutt

# Prompt
PS1='\W λ '
if [ -f ~/.bash_prompt ]; then
  . ~/.bash_prompt
fi
