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

PATH_BASE="\
:$HOME/bin\
:$HOME/.cabal/bin\
:/usr/local/bin\
:/usr/bin\
:/bin\
:/usr/local/sbin\
:/usr/sbin\
:/sbin\
:/usr/local/games\
:/usr/games\
"

export GHC_HP_VERSION="7.6.3"
PATH_GHC_HP="\
:$HOME/.cabal-$GHC_HP_VERSION/bin\
:$HOME/.ghc-$GHC_HP_VERSION/bin\
$PATH_BASE\
"

export PATH="$PATH_GHC_HP"

export HD='/media/Charybdis/zuserm'
alias  HD="cd $HD"
alias ply="cd ~/install/root-files/usr/share/plymouth/themes/custom"
export TEXINPUTS=".:"

alias ls='ls --color=auto'
alias l='l -alh'
alias ll='l'
alias ld='l -d'
alias cl='. cl'
alias grep='grep --color=auto'
alias hat='highlight --out-format=ansi --force'
alias codegrep='grep -RIhA'
alias rmplayer='rm'
alias tags='id3v2 -l'
alias lk='sudo chown -R root:root'
alias ulk='sudo chown -R zuserm:zuserm'
alias printers='sudo system-config-printer'
alias evi='spawn evince'

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
function vims { vim `which $1` ; }

# autologin
if [ -z "$DISPLAY" ] && [ $(tty) == "/dev/tty8" ]; then
  exec startx
fi

