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

if [[ $HOST == "wl-sbothwe-mac" ]]; then
    source $HOME/.profile
fi

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

# Zsh Configuration

# Completion
setopt AUTO_MENU ALWAYS_TO_END AUTO_LIST NO_MENU_COMPLETE COMPLETE_IN_WORD NOMATCH
unsetopt FLOW_CONTROL
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"

# Compinit
autoload -Uz compinit
compinit

# Zsh History
export HISTFILE=${HOME}/.zsh/history
HISTSIZE=50000
SAVEHIST=10000
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt inc_append_history
setopt share_history

# Zsh Plugins
source ${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ${HOME}/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ${HOME}/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh

# Autosuggest Config
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=3"

# Prompting
setopt PROMPT_SUBST

# Bindings
bindkey ' ' magic-space
bindkey '^l' autosuggest-accept
bindkey '^h' autosuggest-clear
bindkey '^k' history-substring-search-up
bindkey '^j' history-substring-search-down
bindkey "${terminfo[kcbt]}" reverse-menu-complete

eval "$(direnv hook zsh)"
eval "$(starship init zsh)"
