# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

export HISTSIZE=1000000

# jni binding for JavaHL subversion client {svn connector for eclipse}
export LD_LIBRARY_PATH=/usr/lib/jni

export LIBXCB_ALLOW_SLOPPY_LOCK=1
export AWT_TOOLKIT=MToolkit

prependPath() {
  case $PATH in
    $@:* | *:$@ | *:$@:* ) ;;
    *) export PATH=$@:$PATH
  esac
}
prependPath $HOME/bin
prependPath $HOME/.cabal/bin
prependPath $JAVA_HOME/bin

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

setxkbmap -option terminate:ctrl_alt_bksp

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
xterm-color)
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    ;;
*)
    PS1='${debian_chroot:+($debian_chroot)}\u:\w\$ '
    ;;
esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    CMD1='echo -ne "\033]0;$WINDOW_TITLE\007"'
    CMD2='echo -ne "\033]0;Terminal: ${PWD/$HOME/~}\007"'
    PROMPT_COMMAND="if [ -n \"\$WINDOW_TITLE\" ]; then $CMD1; else $CMD2; fi"
    unset CMD1
    unset CMD2
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
alias :q='exit'
alias suod='sudo'
alias sudp='sudo'
alias exot='exit'
alias exut='exit'
alias l='ls -al --color=auto'
alias ll='ls -al --color=auto'
alias ld='ls -dal --color=auto'
alias mplayer='echo -ne "\033]0;MPLAYER\007"; mplayer'

alias migl=miglatest
git() {
  CMD=$1
  shift
  if [ $CMD == "co" ]; then
    CMD="clone"
  elif [ $CMD == "ci" ]; then
    CMD="commit"
  fi
  command git $CMD "$@"
}

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

