#!/bin/bash

echo; echo;
read -p "Move home bak stuff (y/N)?"
if [ "$REPLY" == "y" ]; then
  read -p "Use /home/wolke_bak (Y/n)?"
  if [ "$REPLY" == "n" ]; then
    read -p "What shall i use, then?: "
    DIR=$REPLY
  else
    DIR=/home/wolke_bak
  fi

  mkdir ~/.ssh
  mv $DIR/.ssh/* ~/.ssh
  
  mv $DIR/apps ~/
  mv $DIR/Backup ~/
  mv $DIR/.bashrc ~/
  mv $DIR/bin ~/
  mv $DIR/compiz.profile ~/
  mv $DIR/conf ~/
  mv $DIR/.cvoicecontrol ~/
  mv $DIR/.deathnote-bookmark ~/
  rmdir ~/Desktop/*
  rmdir ~/Desktop
  mv $DIR/Desktop ~/
  mv $DIR/.devilspie ~/
  mv $DIR/.gnome-panel ~/
  mv $DIR/.gvimrc ~/
  mv $DIR/install.sh ~/

  mkdir -p ~/.local/share
  mv $DIR/.local/share/rhythmbox ~/.local/share/

  mkdir -p ~/.config/autostart
  mv $DIR/.config/autostart/nautilus.desktop ~/.config/autostart/

  mv $DIR/.mozilla ~/
  mv $DIR/.mozilla-thunderbird ~/
  mv $DIR/openvpn ~/
  mv $DIR/.thunderbird ~/
  mv $DIR/.purple ~/
  mv $DIR/.profile ~/
  mv $DIR/resolvconf ~/
  mv $DIR/resolv.conf ~/
  mv $DIR/.Skype ~/
  mv $DIR/.squirrel-sql ~/
  mv $DIR/.sudoers_default ~/
  mv $DIR/.twip ~/
  mv $DIR/.twiplog ~/
  mv $DIR/.vimrc ~/
  mv $DIR/.wine ~/
  mv $DIR/workspace ~/
  mv $DIR/.xinitrc ~/
  mv $DIR/.Xmodmap ~/
fi


echo; echo;
echo Disabling forced compiz plugins
mkdir -p ~/.config/compiz
echo "COMPIZ_PLUGINS=\"\"" > ~/.config/compiz/compiz-manager


echo; echo;
echo "e.g.: wolke-t400, wolke-t60, wolke-blue, wolke-n900, wolk-desktop"
read -p "hostname='$HOSTNAME' Type a new hostname (y/N)?"
if [ "$REPLY" == "y" ]; then
  OLD=$HOSTNAME
  read -p "type hostname:"
  NEW=$REPLY
  echo $NEW | sudo tee /etc/hostname
  cat /etc/hosts | sed s/$OLD/$NEW/g | sudo tee /etc/hosts
fi

echo; echo;
read -p "Configure keyboard (y/N)?"
if [ "$REPLY" == "y" ]; then
  read -p "Make CapsLock additional Escape key (y/N)?"
  if [ "$REPLY" == "y" ]; then
    setxkbmap -option caps:escape
  fi
  read -p "Ctrl+Alt+Backspace to kill xsession (y/N)?"
  if [ "$REPLY" == "y" ]; then
    setxkbmap -option terminate:ctrl_alt_bksp
  fi
  echo; echo;
  read -p "Run keybindings (y/N)?"
  if [ "$REPLY" == "y" ]; then
    keybindings
  fi;
fi;


echo; echo;
read -p "software-properties-gtk to toggle partner/multiverse/universe? (y/N)"
if [ "$REPLY" == "y" ]; then
  gksu software-properties-gtk
fi;

echo; echo;
read -p "Add third party repos (y/N)?"
if [ "$REPLY" == "y" ]; then
  #NetworkManager and nm-applet
  sudo add-apt-repository ppa:network-manager/trunk
  
  #pidgin stable releases
  sudo add-apt-repository ppa:pidgin-developers/ppa

  #wine unstable developer releases
  sudo add-apt-repository ppa:ubuntu-wine/ppa

  #chrome stable daily releases
  sudo add-apt-repository ppa:chromium-daily/stable

  #thunderbird stable
  sudo add-apt-repository ppa:mozillateam/thunderbird-stable

  echo Add the below for Mesa, OpenGL, xorg drivers nouveau/ati/intel
  read -p "add bleeding edge open source gfx drivers ppa (y/N)?"
  if [ "$REPLY" == "y" ]; then
    #mesa bleeding edge
    sudo add-apt-repository ppa:oibaf/graphics-drivers
  fi

  read -p "ADD KERNEL PPA (y/N)?"
  if [ "$REPLY" == "y" ]; then
    #kernel bleeding edge
    sudo add-apt-repository ppa:kernel-ppa/ppa
  fi
fi

echo; echo;
read -p "sync /var/cache/apt with ~/apt-cache (y/N)?"
if [ "$REPLY" == "y" ]; then
  A="/var/cache/apt"
  B="$HOME/apt-cache"
  sudo rsync -av $A/ $B
  sudo rsync -av $B/ $A
  sudo chown -R $USER.$USER $B
  sudo chown -R root.root $A
fi

echo; echo;
read -p "apt-get update and install free software? (y/N)?"
if [ "$REPLY" == "y" ]; then
  echo; echo;
  echo Updating packages info...
  sudo apt-get update

  echo; echo;
  echo Installing free software packages...
  sudo apt-get install -y vim-gnome
  sudo apt-get install -y openvpn
  sudo apt-get install -y ssh curl gparted pidgin thunderbird \
    compiz-fusion-plugins-extra compizconfig-settings-manager \
    ghc cabal-install ant subversion alltray xmacro mplayer vlc python-pygame \
    nautilus-gksu hwinfo chromium-browser wine1.2 \
    xsel flac libsvn-java xtightvncviewer x11vnc gvfs-bin git \
    gnome-common ttf-inconsolata gimp ffmpeg wmctrl xsane php5 \
    libxslt1-dev libgdbm-dev rhythmbox link-monitor-applet gnome-do \
    librsvg2-bin fbreader xdotool powertop
  sudo apt-get install -y alarm-clock-applet
  sudo apt-get install -y aptitude
fi

echo; echo;
read -p "Install i7z? (y/N)?"
if [ "$REPLY" == "y" ]; then
  echo Installing libncurses5-dev and latest i7z from svn
  install-i7z
fi

echo; echo;
read -p "Install pidgin pipe-notification plugin? (y/N)?"
if [ "$REPLY" == "y" ]; then
  install-pidgin-pipe-notification
fi

echo; echo;
read -p "install xmonad and dzen2? (y/N)"
if [ "$REPLY" == "y" ]; then
  sudo apt-get install xmonad libghc6-xmonad-contrib-dev

  $HOME/bin/install-dzen2

  #move the %-named files in .xmonad to its place
  for file in `ls $HOME/.xmonad/%*`; do
    loc=`echo $file | sed 's/[^%]*%/%/' | sed 's/%/\//g'`
    echo copying $file to $loc
    sudo cp $file $loc
  done
fi;

echo; echo;
read -p "Incude non-free software (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo apt-get update

  echo
  echo Installing non-free software packages...
  sudo apt-get install ubuntu-restricted-extras \
    sun-java6-jdk sun-java6-source sun-java6-plugin

  echo
  echo installing skype
  sudo apt-get install skype

  echo
  echo maybe installing ia32 packages for 64-bit linux
  sudo apt-get install ia32-sun-java6-bin

  echo
  echo Installing non-free encrypted dvd reader
  sudo /usr/share/doc/libdvdread4/install-css.sh
fi

echo; echo;
read -p "Upgrade packages? (y/N)?"
if [ "$REPLY" == "y" ]; then
  echo Upgrading packages
  sudo apt-get upgrade
fi


echo; echo;
read -p "Upgrade packages with aptitude {includes new installs and heldback} (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo aptitude upgrade
fi



echo; echo;
read -p "Install compa (displays command output for gnome-panel) (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo apt-get install libpanel-applet2-dev build-essential
  cd
  cp ~/Desktop/Software/compa-*.tar.gz .
  if [ $? ]; then
    wget 'http://compa.googlecode.com/files/compa-0.98.tar.gz'
  fi
  tar -xf compa-*.tar.gz
  rm compa-*.tar.gz
  cd compa-*
  ./configure --prefix=/usr
  make
  sudo make install
  cd ..
  rm -rf compa-*
  killall gnome-panel
fi;


echo; echo;
read -p "replace compiz wallpaper plugin with a patched one that works (y/N)?"
if [ "$REPLY" == "y" ]; then
  TAR=`echo ~/Desktop/Software/wallpaper-compiz-workaround.tar.bz2`
  echo "Downloaded from http://www.mediafire.com/?usji79x5yiu5d3z"
  echo "workaround from http://forum.ubuntu-fr.org/viewtopic.php?id=421649"
  if [ -e $TAR ]; then
    echo "using: $TAR"
    mkdir /tmp/wallpaper
    cd /tmp/wallpaper
    tar -xf $TAR
    cd /usr/lib/compiz
    SEX=`date +%s`
    sudo mv libwallpaper.a libwallpaper.a_$SEX
    sudo mv libwallpaper.la libwallpaper.la_$SEX
    sudo mv libwallpaper.so libwallpaper.so_$SEX
    sudo cp /tmp/wallpaper/usr/lib/compiz/libwallpaper* .
    rm -rf /tmp/wallpaper
  else
    echo "fetch the file above and put it at: $TAR"
  fi
fi


echo; echo;
bt_disable_cmd="echo disable > /proc/acpi/ibm/bluetooth"
read -p "append '$bt_disable_cmd' to /etc/init.d/rc.local (y/N)?"
if [ "$REPLY" == "y" ]; then
  cmd="\n#turn off bluetooth\n$bt_disable_cmd"
  sudo bash -c "echo -e '$cmd' >> /etc/init.d/rc.local"
fi

echo; echo;
read -p "install tp-smapi {thinkpad battery module} (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo apt-get install -y tp-smapi-dkms
  sudo ~/bin/tp-smapi-install

  echo; echo;
  read -p "install tpbattstat-applet (y/N)?"
  if [ "$REPLY" == "y" ]; then
    ~/bin/build-tpbattstat-applet
  fi
fi

echo; echo;
read -p "Build cvoicecontrol at ~/apps/cvoicecontrol-0.9alpha/ (y/N)?"
if [ "$REPLY" == "y" ]; then
  cd ~/apps/cvoicecontrol-0.9alpha/
  configure
  make
  sudo make install
  cd
fi

echo; echo;
read -p "Setup dnscache with dnsmasq (y/N)?"
if [ "$REPLY" == "y" ]; then
  /home/wolke/bin/config_dnscache
fi

echo; echo;
echo remove indicator-me, indicator-session, and indicator-messages
read -p "remove everything in indicator-applet except volume (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo apt-get remove indicator-messages indicator-me indicator-session
fi


echo; echo;
read -p "install silkscreen font (y/N)?"
if [ "$REPLY" == "y" ]; then
  SS_DIR=/usr/share/fonts/truetype/ttf-silkscreen
  sudo mkdir -p $SS_DIR
  cd $SS_DIR
  sudo wget http://www.kottke.org/plus/type/silkscreen/download/silkscreen.zip
  sudo unzip silkscreen.zip
  sudo rm silkscreen.zip
  sudo fc-cache -fv
fi


echo; echo;
read -p "remove overlay scrollbar (y/N)?"
if [ "$REPLY" == "y" ]; then
  FILE="/etc/X11/Xsession.d/80overlayscrollbars"
  ARG="export LIBOVERLAY_SCROLLBAR=0"
  sudo bash -c "echo $ARG > $FILE"

  sudo apt-get remove overlay-scrollbar
fi

echo; echo;
read -p "magic-panel-set (y/N)?"
if [ "$REPLY" == "y" ]; then
  magic-panel-set
fi


echo; echo;
echo remove gcj and openjdk java packages, since they aint pleasant
echo you might have these: ant-gcj ant-optional-gcj gcj-4.4-base gcj-4.4-jre-lib libgcj-bc libgcj-common libgcj10 default-jre-headless icedtea-6-jre-cacao openjdk-6-jre-headless openjdk-6-jre-lib
read -p "find out which you actually have and then maybe remove (y/N)?"
if [ "$REPLY" == "y" ]; then
  echo "finding out which you have (wont remove them until you say so)"
  echo you definitely have these:
  sudo apt-get remove -sqq '.*gcj.*|openjdk.*' 2> /dev/null
  echo

  read -p "remove the packages found above? (y/N)?"
  if [ "$REPLY" == "y" ]; then
    sudo apt-get remove '.*gcj.*|openjdk.*'
  fi
fi

echo; echo;
read -p "autoremove? (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo apt-get autoremove
fi



echo; echo;
echo setting up /opt and /var/opt to point to ~/conf
read -p "replace /opt and /var/opt with links ~/conf? (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo rm /opt
  sudo rmdir /opt
  sudo mv -f /opt /opt_bak_`date +%s`
  mkdir -p ~/conf/opt
  sudo ln -s ~/conf/opt /opt

  sudo rm /var/opt
  sudo rmdir /var/opt
  sudo mv -f /var/opt /var/var_opt_bak_`date +%s`
  mkdir -p ~/conf/var-opt
  sudo ln -s ~/conf/var-opt /var/opt
fi

echo; echo;
read -p "install compat-wireless backports for iwlagn 300mbps 802.11n (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo apt-get install linux-backports-modules-compat-wireless-2.6.36-maverick-generic
fi

echo; echo;
echo intel 5300 has n disabled by default
echo options iwlagn 11n_disable=1
echo turning fucking 802.11N on
read -p "enable wireless-N? (y/N)"
if [ "$REPLY" == "y" ]; then
  sudo /home/wolke/bin/wifiN on
fi

echo; echo;
echo wish i could do: ccsm  preferences  import ~/compiz.profile
read -p "run ccsm so you can do it? (y/N)"
if [ "$REPLY" == "y" ]; then
  ccsm
fi

echo; echo;
read -p "Install haskell modules (y/N)?"
if [ "$REPLY" == "y" ]; then
  cabal update
  cabal install regex-pcre-builtin
fi

echo; echo;
echo Install some perl modules:
echo Date::Calc   Net::Twitter   File::Slurp   B::Bytecode   XML::LibXSLT
echo Term::Size::Perl
read -p "install the above perl modules(more prompts for each)? (y/N)?"
if [ "$REPLY" == "y" ]; then
  echo we need libssl-dev to install the perl packages below
  sudo apt-get install libssl-dev

  PERL_MM_USE_DEFAULT=1

  echo upgrading CPAN module
  sudo cpan CPAN

  echo setting up Config.pm to make install faster and more autonomous
  CPAN=/etc/perl/CPAN/Config.pm
  if [ -e $CPAN ]; then
    #install prerequisites without prompting
    sudo /home/wolke/bin/cpan-oconf $CPAN prerequisites_policy follow
    #limit build cache to 1000mb
    sudo /home/wolke/bin/cpan-oconf $CPAN build_cache 1000
    #reuse the build_dir in later sessions (successful makes not repeated)
    sudo /home/wolke/bin/cpan-oconf $CPAN build_dir_reuse yes
    #if a module wants to temp install a lib, permanently install it
    sudo /home/wolke/bin/cpan-oconf $CPAN build_requires_install_policy yes
    #submit test reports if possible
    sudo /home/wolke/bin/cpan-oconf $CPAN test_report yes
    #if theres a record of a successful test for this machine, skip test
    sudo /home/wolke/bin/cpan-oconf $CPAN trust_test_report_history yes
  fi

  read -p "install Test::Reporter and upgrade all modules in cpan? (y/N)"
  if [ "$REPLY" == "y" ]; then
    sudo cpan Test::Reporter
    sudo perl -MCPAN -e upgrade
  fi

  sudo cpan Date::Calc
  sudo cpan File::Slurp
  sudo cpan Term::Size::Perl

  echo
  read -p "cpan -f -i Net::Twitter? (y/N)"
  if [ "$REPLY" == "y" ]; then
    sudo cpan -f -i Net::Twitter
  fi

  echo
  read -p "cpan -f -i B::Bytecode? (y/N)"
  if [ "$REPLY" == "y" ]; then
    sudo cpan -f -i B::Bytecode
  fi

  echo
  read -p "install XML::LibXSLT? (y/N)"
  if [ "$REPLY" == "y" ]; then
    sudo cpan XML::LibXSLT
  fi
fi

echo; echo;
echo Desktop background for nautilus
read -p "set desktop bg = gasho? (y/N)?"
if [ "$REPLY" == "y" ]; then
  gconftool-2 --type string --set /desktop/gnome/background/picture_filename "/home/wolke/Desktop/Backgrounds/gasho.png"
fi


echo; echo;
read -p "set desktop custom folder icons and remove places in ~/? (y/N)"
if [ "$REPLY" == "y" ]; then
  rmdir ~/Music ~/Documents ~/Downloads ~/Pictures ~/Templates ~/Videos

  mkdir ~/Desktop/Music ~/Desktop/Videos ~/Desktop/Pictures
  mkdir ~/Desktop/Documents ~/Desktop/Literature\ Books\ Manga
  mkdir ~/Desktop/Torrents ~/Desktop/Downloads
  mkdir ~/Desktop/Games ~/Desktop/Science ~/Desktop/Porn
  mkdir ~/Desktop/ISO\ Images ~/Desktop/Final\ Fantasy ~/Desktop/Euler
  set-desktop-folder-icons
fi

echo;
echo Turning off screensaver idle activation
gconftool-2 --set /apps/gnome-screensaver/idle_activation_enabled --type Boolean "False"
gconftool-2 --set /apps/gnome-screensaver/lock_enabled --type Boolean "False"

echo;
echo Default to the text location bar in nautilus, instead of bread crumb buttons
gconftool-2 --set /apps/nautilus/preferences/always_use_location_entry --type Boolean "True"

echo;
echo Setting buttons to :minimize,maximize,close {all on the right}
gconftool-2 --set /apps/metacity/general/button_layout --type String ":minimize,maximize,close"

echo;
echo "Terminal bg/fg -> black/green, menubar off"
gconftool-2 --set /apps/gnome-terminal/profiles/Default/use_theme_colors --type boolean  false
gconftool-2 --set /apps/gnome-terminal/profiles/Default/background_color --type string "#000000000000"
gconftool-2 --set /apps/gnome-terminal/profiles/Default/foreground_color --type string "#0000FFFF0000"
gconftool-2 --set /apps/gnome-terminal/profiles/Default/default_show_menubar --type boolean "False"
gconftool-2 --set /apps/gnome-terminal/profiles/Default/font --type string "Inconsolata Medium 12"
gconftool-2 --set /apps/gnome-terminal/profiles/Default/use_system_font --type boolean false

echo;
echo Setting monospace font to inconsolata
gconftool-2 --set /desktop/gnome/interface/monospace_font_name --type string "Inconsolata Medium 12"

echo; echo;
echo tapclick off, dont disable touchpad while typing, dont mouse scroll
gconftool-2 --set /desktop/gnome/peripherals/touchpad/tap_to_click --type Boolean "False"
gconftool-2 --set /desktop/gnome/peripherals/touchpad/disable_while_typing --type Boolean "False"
gconftool-2 --set /desktop/gnome/peripherals/touchpad/scroll_method --type Integer "0"
gconftool-2 --set /desktop/gnome/peripherals/touchpad/horiz_scroll_enabled --type Boolean "False"


echo; echo;
echo power mgr: do nothing on crit battery, blank screen on lid close, etc.
gconftool-2 --set /apps/gnome-power-manager/notify/discharging --type Boolean "False"
gconftool-2 --set /apps/gnome-power-manager/actions/critical_battery --type String "nothing"
gconftool-2 --set /apps/gnome-power-manager/actions/critical_ups --type String "nothing"
gconftool-2 --set /apps/gnome-power-manager/actions/low_ups --type String "nothing"
gconftool-2 --set /apps/gnome-power-manager/actions/sleep_type_battery --type String "nothing"
gconftool-2 --set /apps/gnome-power-manager/actions/sleep_type_ac --type String "nothing"
gconftool-2 --set /apps/gnome-power-manager/backlight/battery_reduce --type Boolean "False"
gconftool-2 --set /apps/gnome-power-manager/backlight/enable --type Boolean "False"
gconftool-2 --set /apps/gnome-power-manager/timeout/sleep_computer_ac --type Integer "0"
gconftool-2 --set /apps/gnome-power-manager/timeout/sleep_computer_battery --type Integer "0"
gconftool-2 --set /apps/gnome-power-manager/timeout/sleep_computer_ups --type Integer "0"
gconftool-2 --set /apps/gnome-power-manager/timeout/sleep_display_ac --type Integer "1800"
gconftool-2 --set /apps/gnome-power-manager/timeout/sleep_display_battery --type Integer "600"
gconftool-2 --set /apps/gnome-power-manager/timeout/sleep_display_ups --type Integer "1800"
gconftool-2 --set /apps/gnome-power-manager/buttons/lid_ac --type String "blank"
gconftool-2 --set /apps/gnome-power-manager/buttons/lid_battery --type String "blank"
gconftool-2 --set /apps/gnome-power-manager/disks/spindown_enable_battery --type Boolean "False"
gconftool-2 --set /apps/gnome-power-manager/disks/spindown_enable_ac --type Boolean "False"
gconftool-2 --set /apps/gnome-power-manager/ui/icon_policy --type String "always"

echo; echo;
read -p "add escribe.com servers to /etc/hosts (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo /home/wolke/bin/escribe-hosts
fi

echo; echo;
echo really dangerous: dont say yes, do it yourself
echo add the following line to /etc/sudoers:
echo wolke   ALL = NOPASSWD: /home/wolke/bin/cpu_governor
read -p "overwrite /etc/sudoers, despite your better judgment (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo sh -c "chmod +w /etc/sudoers && echo \"wolke\tALL=(ALL) NOPASSWD: /home/wolke/bin/cpu_governor\" >> /etc/sudoers && chmod -w /etc/sudoers"
fi


echo; echo;
echo really dangerous: dont say yes, do it yourself
echo add the following line to /etc/sudoers:
echo wolke   ALL = NOPASSWD: /home/wolke/bin/sslvpn
read -p "overwrite /etc/sudoers, despite your better judgment? (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo sh -c "chmod +w /etc/sudoers && echo \"wolke\tALL=(ALL) NOPASSWD: /home/wolke/bin/sslvpn\" >> /etc/sudoers && chmod -w /etc/sudoers"
fi

echo; echo;
echo really dangerous: dont say yes, do it yourself
echo add the following line to /etc/sudoers:
echo wolke   ALL = NOPASSWD: /home/wolke/bin/resolvchooser
read -p "overwrite /etc/sudoers, despite your better judgment? (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo sh -c "chmod +w /etc/sudoers && echo \"wolke\tALL=(ALL) NOPASSWD: /home/wolke/bin/resolvchooser\" >> /etc/sudoers && chmod -w /etc/sudoers"
fi



echo; echo;
echo really dangerous: dont say yes, do it yourself
echo add the following line to /etc/sudoers:
echo wolke   ALL = NOPASSWD: /usr/bin/i7z
read -p "overwrite /etc/sudoers, despite your better judgment (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo sh -c "chmod +w /etc/sudoers && echo \"wolke\tALL=(ALL) NOPASSWD: /usr/sbin/i7z\" >> /etc/sudoers && chmod -w /etc/sudoers"
fi

echo; echo;
read -p "fix 32-bit flash {export GDK_NATIVE_WINDOWS} (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo ~/bin/config_flash
fi


echo; echo;
echo 'change /etc/default/grub ->'
echo ' GRUB_HIDDEN_TIMEOUT=3'
echo ' GRUB_HIDDEN_TIMEOUT_QUIET=false'
echo and then run 'update-grub'
read -p "Modify grub (y/N)?"
if [ "$REPLY" == "y" ]; then
  sudo ~/bin/config_grub
  sudo update-grub
fi

