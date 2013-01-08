[ -f /etc/bashrc ] && . /etc/bashrc
[ -f /etc/bash_completion ] && . /etc/bash_completion

shopt -s histappend
HISTCONTROL=ignoredups:ignorespace
HISTSIZE=1000000

shopt -s checkwinsize
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

case "$TERM" in rxvt*) TERM=rxvt ;; esac

if [ "$TERM" == "rxvt" ]; then
  p1='echo -ne "\033]0;$WINDOW_TITLE\007"'
  p2='echo -ne "\033]0;Terminal: ${PWD/$HOME/~}\007"'
  PROMPT_COMMAND='if [ "$WINDOW_TITLE" ]; then '$p1'; else '$p2'; fi'
fi

PS1="\[\033[G\]$PS1"

function prependPath {
  case $PATH in
    $@:* | *:$@ | *:$@:* ) ;;
    *) export PATH=$@:$PATH
  esac
}
#prependPath "$HOME/.ghc/bin"
#prependPath "$HOME/.ghc-7.4.2/bin"
#prependPath "$HOME/.ghc-7.6.1/bin"
prependPath "$HOME/.cabal/bin"
prependPath "$HOME/bin"
prependPath "$HOME/bin/phon"

export HD='/media/Charybdis/zuserm'

alias ls='ls --color=auto'
alias l='ls --group-directories-first'
alias ll='l -alh'
alias ld='ll -d'
alias cl='. cl'
alias grep='grep --color=auto'
alias hat='highlight --out-format=ansi --force'
alias codegrep='grep -RIhA'
alias rmplayer='rm'
alias tags='id3v2 -l'
alias lk='sudo chown -R root:root'
alias ulk='sudo chown -R zuserm:zuserm'
alias printers='sudo system-config-printer'

alias :l='ghci'
alias :h='man'
alias :q='exit'
alias :r='. /etc/profile ; . ~/.bashrc'

alias cbi='spawn chromium-browser --incognito'
function tex2pdf { pdflatex -halt-on-error "$1".tex && evince "$1".pdf ; }

#alias o='gnome-open'

function spawn { "$@" & disown ; }
function spawnex { "$@" & disown && exit 0 ; }
complete -F _root_command spawn spawnex

alias evince='spawn evince'


if [ -z "$DISPLAY" ] && [ $(tty) == "/dev/tty7" ]; then
  exec startx
fi

