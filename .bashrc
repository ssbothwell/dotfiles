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
alias mutt=neomutt
alias ls=exa

# Functions
sc () { du -a  ~/.bin | awk '{print $2}' | fzf | parallel -j1 --tty $EDITOR; }
dc () {
  if [ -z $1 ]
  then :
    cd `cat ~/.bin/bookmarks | fzf | awk '{print $2}' | sed "s/~/\/home\/solomon/"`;
  else :
    if [ $1 == "add" ]
    then :
      echo "Added: '$2 ${PWD}' to bookmarks.";
      echo "$2 ${PWD}" >> "${HOME}/.bin/bookmarks";
  # else :
  # if [ $1 == "del" ]
  # then :
  #   <find and delete a bookmark
    fi
  fi
}

#td ()
#  add a new todo item or use fzf to select a todo item to either:
#    1. mark complete
#    2. delete
#    3. modify

# Prompt
PS1='\W λ '
if [ -f ~/.bash_prompt ]; then
  . ~/.bash_prompt
fi
