##########
# .zshrc #
##########
alias emacs='emacs -fs'
alias grep='grep --color'
alias l='ls -lh'
alias ls='ls --color'
alias ll='ls -lha'

autoload -U bashcompinit && bashcompinit
autoload -U compinit && compinit
autoload -U zcalc
autoload colors && colors

bindkey -e

setopt auto_cd
setopt extendedglob
setopt extendedhistory
setopt hist_ignore_all_dups
setopt menu_complete
setopt no_beep
setopt no_case_glob
setopt numeric_glob_sort
setopt prompt_subst
setopt pushd_minus
setopt share_history

HISTFILE="$HOME/.zshrc_history"
HISTSIZE=10000
PROMPT='%B%{%(#~%F{red}~%F{black})%}%n %# %f%b'
RPROMPT='${${(%):-%d}//\//%B%F{black\}/%B%F{blue\}}%f %F{green}%D{%H:%M:%S.%.}%f%b'
SAVEHIST=10000

zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' completer _complete _correct _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-tab false
zstyle ':completion:*' rehash true
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'

[[ $UID != 0 || $EUID != 0  ]] && xmodmap "$HOME/.xmodmap.conf"
export PATH="$PATH:$HOME/documents/scripts"
