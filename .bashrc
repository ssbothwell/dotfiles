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

# PROMPT
PS1='\[${MAGENTA}\]\u@\h \[${YELLOW}\]\W \$ '

# VI mode
set -o vi

# Colorize ls
if [ -f ~/.dir_colors ]; then
    eval `dircolors ~/.dir_colors`
fi
alias ls='ls --color=auto'

# PATH additions
export PATH="${HOME}/.bin:${PATH}:~/.node_modules/bin:~/.local/bin"

# Set Vim as default editor (for git etc)
export VISUAL=vim
export EDITOR="$VISUAL"

# Disable XOFF
stty -ixon
source /usr/bin/activate.sh

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/solomon/google-cloud-sdk/path.bash.inc' ]; then source '/home/solomon/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/solomon/google-cloud-sdk/completion.bash.inc' ]; then source '/home/solomon/google-cloud-sdk/completion.bash.inc'; fi

# Set up Node Version Manager
source /usr/share/nvm/init-nvm.sh
