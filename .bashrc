[ -f /etc/bashrc ] && . /etc/bashrc
[ -f /etc/bash_completion ] && . /etc/bash_completion

shopt -s histappend
HISTCONTROL=ignoredups:ignorespace:ignoreboth
HISTSIZE=1000000

export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/

shopt -s checkwinsize
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
ssh-add ~/.ssh/id_rsa 2> /dev/null

case "$TERM" in rxvt*) TERM=rxvt ;; esac

if [ "$TERM" == "rxvt" ]; then
  p1='echo -ne "\033]0;$WINDOW_TITLE\007"'
  p2='echo -ne "\033]0;Terminal: ${PWD/$HOME/~}\007"'
  PROMPT_COMMAND='if [ "$WINDOW_TITLE" ]; then '$p1'; else '$p2'; fi'
fi

PS1="\[\033[G\]$PS1"

pathAppend ()  { for x in $@; do pathRemove $x; export PATH="$PATH:$x"; done }
pathPrepend () { for x in $@; do pathRemove $x; export PATH="$x:$PATH"; done }
pathRemove ()  { for x in $@; do
  export PATH=`\
    echo -n $PATH \
    | awk -v RS=: -v ORS=: '$0 != "'$1'"' \
    | sed 's/:$//'`;
  done
}

pathAppend          \
  $HOME/.cabal/bin  \
  /usr/local/bin    \
  /usr/bin          \
  /bin              \
  /usr/local/sbin   \
  /usr/sbin         \
  /sbin             \
  /usr/local/games  \
  /usr/games        \
;

export GHC_HP_VERSION="7.6.3"
pathPrepend  \
  $HOME/.cabal-$GHC_HP_VERSION/bin  \
  $HOME/.ghc-$GHC_HP_VERSION/bin    \
  $HOME/bin                         \
;

export HD='/media/Charybdis/zuserm'
alias  HD="cd $HD"
alias ply="cd ~/install/root-files/usr/share/plymouth/themes/custom"
export TEXINPUTS=".:"

alias ls='ls --color=auto'
alias l='ls -alh'
alias ll='l'
alias ld='l -d'
alias cl='. cl'
alias g='git'
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

for cmd in wconnect wauto tether resolv \
           mnt optimus xorg-conf bluetooth fan intel-pstate flasher
do alias $cmd="sudo $cmd"; done

function spawn { "$@" & disown ; }
function spawnex { "$@" & disown && exit 0 ; }
complete -F _root_command spawn spawnex
function vims { vim `which $1` ; }
function update-repo { sudo apt-get update \
                         -o Dir::Etc::sourcelist="sources.list.d/$1" \
                         -o Dir::Etc::sourceparts="-" \
                         -o APT::Get::List-Cleanup="0"
}
function git-log(){ git ln $@ ; }
function git()
{
  realgit="$(which git)"
  cmd="git-$1"
  if [ "$(type -t $cmd)" = "function" ]; then
    shift
    $cmd "$@"
  else
    $realgit "$@"
  fi
}

# allow <C-S> in vim
stty stop undef
