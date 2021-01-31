[ -f /etc/bashrc ] && . /etc/bashrc
[ -n "$PS1" ] && [ -f /etc/bash_completion ] && . /etc/bash_completion
[ -n "$PS1" ] && [ -f /etc/bash_completion.d/git ] && . /etc/bash_completion.d/git
[ -n "$PS1" ] && [ -f ~/.bash_completion ] && . ~/.bash_completion

shopt -s dotglob
shopt -s extglob

shopt -s checkwinsize # update LINES and COLUMNS based on window size

# allow <C-S> in vim
stty stop undef 2>/dev/null

ssh-add ~/.ssh/id_rsa 2> /dev/null

export TCLLIBPATH="${HOME}/.local/share/tk-themes"

export QUOTING_STYLE=literal #the fuck? fucken coreutils man
export HISTTIMEFORMAT="%F %T "
export HISTSIZE=1000000
# ignoredups: do not add duplicate history entries
# ignoredspace: do not add history entries that start with space
export HISTCONTROL=ignoredups:ignorespace
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/

export GTK_OVERLAY_SCROLLING=0

[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)" #less on binary files, e.g. tars

#rxvt-unicode and rxvt-256color => rxvt {for legacy}
case "$TERM" in rxvt*) TERM=rxvt ;; esac

#use prompt_cmd to set the window title => $WINDOW_TITLE or "Terminal: pwd"
#only for rxvt* terms
if [ "$TERM" == "rxvt" ]; then
  p1='echo -ne "\033]0;$WINDOW_TITLE\007"'
  p2='echo -ne "\033]0;Terminal: ${PWD/$HOME/~}\007"'
  PROMPT_COMMAND='if [ "$WINDOW_TITLE" ]; then '$p1'; else '$p2'; fi'
fi

rm -f .viminf*.tmp .recently-used #clean home

###horrible fucking oracle variables
if [[ -z "$ORACLE_HOME" ]] && [[ -f /etc/ld.so.conf.d/oracle.conf ]]; then
  oralibdir=`cat /etc/ld.so.conf.d/oracle.conf`
  export ORACLE_HOME=`dirname "$oralibdir"`
fi
if [[ -z "$SQLPATH" ]] && [[ -n "$ORACLE_HOME" ]]; then
  export SQLPATH=$ORACLE_HOME/lib
fi
###

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
  $HOME/bin         \
  $HOME/.cabal/bin  \
  /usr/local/bin    \
  /usr/bin          \
  /bin              \
  /usr/local/sbin   \
  /usr/sbin         \
  /sbin             \
  /usr/local/games  \
  /usr/games        \
  /snap/bin         \
;

host_alias=`hostname | cut -f 1,2 -d '.'`
hostname="$host_alias"

########################################
# command prompt
if [[ -z "$DISPLAY" ]]; then
  #host abbrevs
  case "$host_alias" in
    "wolke-main"              ) h='@main' ;;
    "wolke-aux"               ) h='@aux' ;;
    "wolke-bed"               ) h='@bed' ;;
    "wolke-nuc"               ) h='@nuc' ;;
    "wolke-sx"                ) h='@sx' ;;
    "raspberrypi"             ) h='@raspi' ;;
    *                         ) h="@$host_alias" ;;
  esac

  #set DISPLAY using who (probably ":0")
  export DISPLAY=`display-guess`
else
  #if display was set, you probably know where you are
  h=""
fi

u="\u"
c1='\[\033[01;32m\]'
c2='\[\033[01;34m\]'
cEnd='\[\033[00m\]'
if [ -n "PS1" ]; then
  PS1="$c1$u$h$cEnd:$c2\w$cEnd\$ "
fi

########################################
# aliases
for cmd in wconnect wauto tether resolv \
           mnt optimus xorg-conf bluetooth fan intel-pstate flasher \
           tpacpi-bat sbox-umount
do alias $cmd="sudo $cmd"; done

for sudoTypo in suod sudp
do alias $sudoTypo='sudo'; done

for exitTypo in exot exut
do alias $exitTypo='exit'; done

alias time="command time"
alias mkdir="mkdir -p"
alias :q='exit'
alias :r='. /etc/profile; . ~/.bashrc;'
alias r='stty sane'

IPMAGIC_DIR="$HOME/.config/ipmagic"
IPMAGIC_CONF_FILES=$(ls $IPMAGIC_DIR/*.conf 2>/dev/null)
IPMAGIC_NAMES=$(basename --suffix=.conf -a $IPMAGIC_CONF_FILES 2>/dev/null)

alias ipm=ipmagic
for ipmagicName in $IPMAGIC_NAMES
do
  alias ipm$ipmagicName="ipmagic $ipmagicName";
  alias $ipmagicName="ipmagic $ipmagicName";
  alias ${ipmagicName}s="ipmagic $ipmagicName -s";
done

function e            { email-summary "$@" 2>&1 | less -S; }
function eu           { email.pl --update "$@"; }
function ds           { zenius-ddrsonglist --search "$@"; }
function ddr-search   { zenius-ddrsonglist --search "$@"; }
function ddr-banners  { image-flash $(zenius-ddrsonglist --banners "$@"); }
function ddredit      { ddrname --edit-names --fast "$@"; }
function j            { fcron-job-toggle "$@"; }
function f            { feh "$@"; }
function snapshot     { backup --snapshot "$@"; }
function qgroups-info { backup --info --quick --sort-by=size "$@"; }
function dus          { du -s * | sort -g "$@"; }
function killjobs     { kill -9 `jobs -p` 2>/dev/null; sleep 0.1; echo; }
function gvim         { term vim "$@"; }
function cx           { chmod +x "$@"; }
function shutdown     { poweroff "$@"; }
function xmb          { xmonad-bindings "$@"; }
function ls           { command ls --color=auto "$@"; }
function l            { command ls -Al --color=auto "$@"; }
function ll           { command ls -Al --color=auto "$@"; }
function ld           { command ls -dAl --color=auto "$@"; }
function perms        { stat -c %a "$@"; }
function glxgears     { vblank_mode=0 glxgears "$@"; }
function mnto         { sudo mnt --other --no-usb --no-card "$@"; }
function gparted      { spawnexsudo gparted "$@"; }
function escape-pod   { ~/Code/escapepod/escape-pod-tool --escapepod "$@"; }
function podcastle    { ~/Code/escapepod/escape-pod-tool --podcastle "$@"; }
function pseudopod    { ~/Code/escapepod/escape-pod-tool --pseudopod "$@"; }
function g            { git "$@"; }
function gs           { git s "$@"; }
function gss          { git ss "$@"; }
function mp           { mpv "$@"; }
function mpu          {
  if [ -z $2 ] ; then local default_quality='best' ; fi
  livestreamer "$@" $default_quality
}

function vol {
  # use pulse-vol unless theres a command named `vol`
  VOL_CMD=$(which vol)
  if [ -n "$VOL_CMD" ]; then
    $VOL_CMD "$@"
  else
    pulse-vol "$@"
  fi
}

function rename       {
  pastOptArgs=0
  ok=1
  for arg in "$@"
  do
    if [[ $pastOptArgs == 1 && $arg == -* ]]; then
      echo "NOT RUNNING RENAME, $arg MUST BE BEFORE PATTERN"
      ok=0
    fi
    if [[ $arg != -* ]]; then
      pastOptArgs=1
    fi
  done
  if [[ $ok == 1 ]]; then
    command rename "$@"
  fi
}

function sb           { seedbox "$@"; }
function sbr          { seedbox -r "$@"; }
function sbw          { seedbox -r ssh wolke@192.168.11.50 "$@"; }
function sbrsync      { seedbox --rsync-revtun "$@"; }
function sbs          { sb-rt-status "$@"; }
function sd           { screen-daemon "$@"; }
function sbd          { screen-daemon sb-daemon "$@"; }
function sb-daemon    { screen-daemon sb-daemon "$@"; }

function rsyncsshc    { sshc --rsync "$@"; }

function k            { pkill -9 keys ; keys-to-window; }

function sm           { sheetmusic "$@"; }

function pl           { play-show "$@"; }

function ts           { tasmota "$@"; }

function igcmd        { if [[ "$(hostname)" == "raspberrypi" ]]; then
                          command igcmd "$@";
                        elif [[ "$1" == "--bash-complete" ]]; then
                          command igcmd "$@";
                        else
                          echo "RUNNING ON raspi"
                          ipmagic raspi -b "command igcmd $@"
                        fi
                      }
function ig           { igcmd "$@"; }
function acfan_up     { igcmd ac-fan_up "$@"; }
function acfan_down   { igcmd ac-fan_down "$@"; }
function acpower      { igcmd ac-power "$@"; }
function ac2fan_up    { igcmd ac2-fan_up "$@"; }
function ac2fan_down  { igcmd ac2-fan_down "$@"; }
function ac2power     { igcmd ac2-power "$@"; }

function dfavail      { df-tool avail "$@"; }
function dfused       { df-tool used "$@"; }

function tvg          { tv-gpio "$@" ; }

function so           { screenOff ; }

function s            { "$@" & disown; }
function spawn        { "$@" & disown; }
function spawnex      { "$@" & disown && exit 0; }
function spawnexsudo  { gksudo "$@" & disown && exit 0; }

function m            { maven -Psdm -Pdev -Pfast-tests -Dgwt.compiler.skip=true install "$@"; }
function mdebug       { mavenDebug -Psdm -Pdev -Dgwt.compiler.skip=true "$@"; }
function mc           { maven -Psdm -Pdev -Pfast-tests -Dgwt.draftCompile=true clean install "$@"; }
function mck          { maven checkstyle:check "$@"; }
function findesh      { command find "$@" -not -regex '\(^\|.*/\)\(target\|gen\|compile-sql\)\($\|/.*\)'; }
function grepesh      { command grep "$@" \
                            --exclude-dir=.git \
                            --exclude-dir=target \
                            --exclude-dir=gen \
                            --exclude-dir=compile-sql \
                            --exclude pdf.worker.js.map \
                            --exclude Words.java \
                            ;
                      }
function rebase       { rebase-reltime -e "$@" --same; }

function genservices  { ~/workspace/escribehost/legacy-tools/genservices.pl "$@"; }
function genibatis    { ~/workspace/escribehost/legacy-tools/genibatis.pl "$@"; }
function migl         { vim `~/workspace/escribehost/migrations/latest-script "$@"`; }

function recscreen    { simplescreenrecorder --start-recording "$@"; }

function first        { ls "$@" | head -1; }
function last         { ls "$@" | tail -1; }
function apN          { let n=${#@}; "$2" "${@:3:$1-1}" "${!n}" "${@:$1+2:$n-$1-2}"; }

# common typos
function mkdit        { mkdir "$@"; }
function cim          { vim "$@"; }
function bim          { vim "$@"; }

# dc DIR => cd DIR
# dc ARG ARG .. => command dc ARG ARG
function dc {
  if [[ $# == 1 ]] && [[ -d $1 ]]; then
    cd "$1"
  else
    command dc "$@"
  fi
}

function maven() {
  args=""
  if ! [[ "$@" =~ (^| )test($| ) ]]; then
    args="$args -DskipTests"
  fi
  if ! [[ "$@" =~ (^| )checkstyle:check($| ) ]]; then
    args="$args -Dcheckstyle.skip=true"
  fi
  echo mvn $args $@
  execAlarm mvn $args $@;
}
function mavenDebug() {
  port="8000"
  debugOpts="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=$port -Xnoagent -Djava.compiler=NONE"
  args=""
  if ! [[ "$@" =~ (^| )test($| ) ]]; then
    args="$args -DskipTests"
  fi
  if ! [[ "$@" =~ (^| )checkstyle:check($| ) ]]; then
    args="$args -Dcheckstyle.skip=true"
  fi
  echo mvn -Dmaven.surefire.debug=\'$debugOpts\' $args $@
  execAlarm mvn -Dmaven.surefire.debug="$debugOpts" $args $@;
}


function find() {
  if [[ "$PWD" =~ "escribe" ]]; then
    findesh "$@"
  else
    command find "$@"
  fi
}

function grep() {
  if [[ "$PWD" =~ "escribe" ]]; then
    grepesh -s "$@"
  else
    command grep -s "$@"
  fi
}

function execAlarm() {
  "$@"
  exitCode="$?"
  if [ $exitCode == 0 ]; then
    alarm -s success
  else
    alarm -s failure
  fi
  bash -c "exit $exitCode"
}

function update-repo {
  repo="$1"
  shift
  sudo apt-get update \
    -o Dir::Etc::sourcelist="sources.list.d/$repo" \
    -o Dir::Etc::sourceparts="-" \
    -o APT::Get::List-Cleanup="0" \
    "$@"
}

function git() {
  cmd="$1"
  shift
  if [[ "$cmd" == log ]]; then
    command git log --color --decorate --name-status -M "$@"
  elif [[ "$cmd" == log-real ]]; then
    command git log "$@"
  else
    command git "$cmd" "$@"
  fi
}
