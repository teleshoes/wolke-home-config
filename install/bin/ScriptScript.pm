package ScriptScript;
use warnings;
use strict;
use String::ShellQuote;
use File::Temp 'tempfile';
require Exporter;
my $IPC_RUN = eval{require IPC::Run};
my $IO_PTY = eval{require IO::Pty};

our @ISA = qw(Exporter);
our @EXPORT_OK = qw(setOpts);
our @EXPORT = qw( getScriptNames getSubNames
                  getInstallNames getInstallScriptNames getInstallSrcNames getInstallPipNames
                  run tryrun
                  shell tryshell
                  runANSI tryrunANSI
                  runUser tryrunUser wrapUserCommand
                  runAptGet tryrunAptGet
                  proc procLines
                  runScript
                  getHome getInstallPath getSrcCache
                  cd
                  symlinkFile
                  which
                  globOne
                  writeFile tryWriteFile writeFileSudo
                  readFile tryReadFile readFileSudo
                  replaceLine replaceOrAddLine
                  editFile editSimpleConf editIni
                  getRoot getRootSu
                  getUsername
                  guessBackupDir
                  readConf readConfDir
                  installFromDir aptSrcInstall removeSrcCache
                  installFromGit removeGitSrcCache extractNameFromGitUrl
                );

sub getScriptNames();
sub getSubNames();
sub setOpts($);
sub deathWithDishonor(;$);
sub withOpenHandle($$$);
sub parseAnsiSequences($);
sub assertDef($@);
sub runProto($@);
sub runProtoIPC($@);
sub runProtoNoIPC($@);
sub run(@);
sub tryrun(@);
sub shell(@);
sub tryshell(@);
sub runUser(@);
sub tryrunUser(@);
sub wrapUserCommand(@);
sub proc(@);
sub procLines(@);
sub runScript($@);
sub getUsername();
sub getInstallPath($);
sub which($);
sub cd($);
sub symlinkFile($$);
sub globOne($);
sub writeFileProto($@);
sub writeFile($$);
sub tryWriteFile($$);
sub writeFileSudo($$);
sub readFileProto($@);
sub readFile($);
sub tryReadFile($);
sub readFileSudo($);
sub replaceLine($$$);
sub replaceOrAddLine($$$);
sub editFile($$;$);
sub editSimpleConf($$$);
sub editIni($$$);
sub getRoot(@);
sub getRootSu(@);
sub guessBackupDir();
sub readConf($);
sub readConfDir($);
sub installFromDir($;$$);
sub aptSrcInstall($$);
sub removeSrcCache($);
sub installFromGit($;$);
sub removeGitSrcCache($);
sub extractNameFromGitUrl($);

$SIG{INT} = sub{ system "rm -f /tmp/progress-bar-*"; exit 130 };

my $opts = {
  putCommand     => 1,
  runCommand     => 1,
  verbose        => 1,
  progressBar    => 1,
  prependComment => 1,
  };

sub getScriptNames(){
    my $bin = getInstallPath "bin";
    my @scripts = `ls $bin/`;
    chomp foreach @scripts;
    @scripts = grep {/^[a-zA-Z0-9_\-]+$/} @scripts;
    @scripts = grep { -f "$bin/$_" } @scripts;
    return \@scripts;
}
sub getSubNames(){
    my @subs = @EXPORT;
    @subs = grep {/^[a-zA-Z0-9_\-]+$/} @subs;
    return \@subs;
}
sub getInstallNames(){
    my @installNames = (
      @{getInstallScriptNames()},
      @{getInstallSrcNames()},
      @{getInstallPipNames()},
    );
    return \@installNames;
}
sub getInstallScriptNames(){
    my @installScriptNames;
    my $scriptNames = getScriptNames;
    for my $scriptName(@$scriptNames){
      push @installScriptNames, $1 if $scriptName =~ /install-(.*)/;
    }
    return \@installScriptNames;
}
sub getInstallSrcNames(){
  my $installSrcCmd = getInstallPath "bin/install-src";

  my @installSrcNames = `$installSrcCmd --list`;
  chomp foreach @installSrcNames;
  return \@installSrcNames;
}
sub getInstallPipNames(){
  my $installPipCmd = getInstallPath "bin/install-pip-packages";

  my @installPipNames = `$installPipCmd --list`;
  chomp foreach @installPipNames;
  return \@installPipNames;
}

sub setOpts($) {
    my %new = (%$opts, %{$_[0]});
    $opts = \%new;
}

sub deathWithDishonor(;$) {
    my $msg = shift;
    $msg = "command failed" if not defined $msg or $msg eq "";
    chomp $msg;
    $msg .= "\n";
    $msg = "## $msg" if $$opts{prependComment};

    print STDERR $msg;
    exit 1;
}

sub withOpenHandle($$$){
  my ($openCmd, $fatal, $withFHSub) = @_;
  my ($mode, @openList) = @$openCmd;
  my $fh;
  if(open $fh, $mode, @openList){
    return &$withFHSub($fh);
  }elsif($fatal){
    deathWithDishonor "open failed: open @$openCmd";
  }
}

sub parseAnsiSequences($){
  my ($str) = @_;
  #re-encode text modes
  $str =~ s/
    \^\[            #escape char
    \[              #bracket char
    ([0-9;]*)       #list of semi-colon separated integers
    m               #m
    /\e\[$1m/gx;
  #strip out all other sequences
  $str =~ s/
    \^\[            #escape char
    \[              #bracket char
    [?(]?           #optional question mark or parens
    [0-9;]*         #list of semi-colon separated integers
    [a-zA-Z]        #control character
    //gx;
  return $str;
}

sub assertDef($@){
  my $h = shift;
  foreach my $key(@_){
    deathWithDishonor "missing arg $key" if not defined $$h{$key};
  }
  my $size = keys %$h;
  deathWithDishonor "too many args" if @_ != $size;
}

sub runProto($@){
    return &{$IPC_RUN && $IO_PTY ? \&runProtoIPC : \&runProtoNoIPC }(@_)
}
sub runProtoIPC($@) {
    my $cfg = shift;
    assertDef $cfg, qw(esc fatal ansi);

    my @cmd = &{$$cfg{esc}}(@_);

    system "rm -f /tmp/progress-bar-*";

    print "@cmd\n" if $opts->{putCommand};
    return     unless $opts->{runCommand};

    my $pty = new IO::Pty();
    my $slave = $pty->slave;
    $pty->blocking(0);
    $slave->blocking(0);
    my $h = IPC::Run::harness(["sh", "-c", "@cmd"], ">", $slave, $pty);
    if($$cfg{fatal}){
        $h->start;
    }else{
        $h = eval {$h->start};
        return if not defined $h;
    }
    my $progFile = "/tmp/progress-bar-" . time . ".txt";

    while($h->pumpable){
        eval { $h->pump_nb }; #eval because pumpable doesnt really work
        my $out = <$pty>;
        if(defined $out and $out ne ""){
            if($opts->{progressBar} and $out =~ /(100|\d\d|\d)%/){
                open my $fh, "> $progFile";
                print $fh "$1\n";
                close $fh;
            }
            $out = "# $out" if $opts->{prependComment};
            $out = parseAnsiSequences $out if $$cfg{ansi};
            chomp $out;
            print "$out\n" if defined $opts->{verbose};
        }
        <$slave>;
    }
    IPC::Run::finish $h;
    system "rm", "-f", $progFile;
    deathWithDishonor if $$cfg{fatal} and $h->result != 0;
    return $h->result == 0;
}
sub runProtoNoIPC($@) {
    my $cfg = shift;
    assertDef $cfg, qw(esc fatal ansi);

    my $cmd = join ' ', &{$$cfg{esc}}(@_);

    print "$cmd\n" if $opts->{putCommand};
    return     unless $opts->{runCommand};

    my $pid = open my $fh, "-|";
    if(not $pid) {
        open(STDERR, ">&STDOUT");
        exec $cmd or deathWithDishonor;
    } else {
        if($opts->{verbose}) {
            while(my $line = <$fh>) {
                chomp $line;
                $line = "# $line" if $opts->{prependComment};
                $line = parseAnsiSequences $line if $$cfg{ansi};
                print "$line\n";
            }
        }
        close $fh;
        deathWithDishonor if $? != 0 and $$cfg{fatal};
        return $? == 0;
    }
}

sub id(@){@_}

sub run       (@) { runProto {esc => \&shell_quote, fatal => 1, ansi => 0}, @_ }
sub tryrun    (@) { runProto {esc => \&shell_quote, fatal => 0, ansi => 0}, @_ }
sub shell     (@) { runProto {esc => \&id         , fatal => 1, ansi => 0}, @_ }
sub tryshell  (@) { runProto {esc => \&id         , fatal => 0, ansi => 0}, @_ }
sub runANSI   (@) { runProto {esc => \&shell_quote, fatal => 1, ansi => 1}, @_ }
sub tryrunANSI(@) { runProto {esc => \&shell_quote, fatal => 0, ansi => 1}, @_ }
sub runUser   (@) { run wrapUserCommand(@_) }
sub tryrunUser(@) { tryrun wrapUserCommand(@_) }

sub runAptGet(@){
  my @cmd = isRoot() ? ("apt-get", @_) : ("sudo", "apt-get", @_);
  runANSI @cmd;
}
sub tryrunAptGet(@){
  my @cmd = isRoot() ? ("apt-get", @_) : ("sudo", "apt-get", @_);
  tryrunANSI @cmd;
}

sub wrapUserCommand(@) {
    return isRoot() ? ("su", getUsername(), "-c", (join ' ', shell_quote @_)) : @_;
}

sub proc(@) {
    my $out = `@_`;
    chomp $out;
    return $out;
}
sub procLines(@) {
    my @lines = `@_`;
    chomp foreach @lines;
    return @lines;
}

sub runScript($@){
  my $scriptName = shift;
  my $script = getInstallPath "bin/$scriptName";
  run $script, @_;
}

sub getUsername() {
    my $user = $ENV{SUDO_USER} || $ENV{USER};
    if(not $user or $user eq "root") {
        deathWithDishonor "ERROR: USER or SUDO_USER must be set and not root";
    }
    $user
}

sub getHome() {
    if(not isRoot()) {
        return $ENV{HOME};
    }else {
        return "/home/" . getUsername();
    }
}

sub getInstallPath($) {
    return getHome() . "/install/$_[0]";
}

sub getSrcCache() {
    return getHome() . "/.src-cache";
}

sub which($) {
    return proc "which", @_;
}

sub cd($) {
    my $path = shift;
    my $escpath = shell_quote $path;
    my $cmd = "cd $escpath";

    print "$cmd\n" if $opts->{putCommand};
    return     unless $opts->{runCommand};

    chdir $path or deathWithDishonor;
}

sub symlinkFile($$) {
    my ($file, $target) = @_;
    if(-l $file){
        my $link = readlink $file;
        run "sudo", "rm", $file;
        if($link ne $target){
            print "  $file: $link => $target\n";
        }
    }elsif(-d $file){
        run "sudo", "rmdir", $file;
        print "  $file => $target\n";
    }
    deathWithDishonor "Could not symlink $file => $target\n" if -e $file;
    run "sudo", "ln", "-s", $target, $file;
}

sub hereDoc($){
  my $s = shift;
  my $delim = "EOF";
  while($s =~ /^$delim$/m){
      $delim .= "F";
  }
  return "<< \"$delim\"\n"
    . "$s\n"
    . "$delim\n";
}

sub globOne($){
  my @files = glob $_[0];
  if(@files == 1){
    return $files[0];
  }else{
    return undef;
  }
}

sub writeFileProto($@) {
    my $cfg = shift;
    assertDef $cfg, qw(sudo fatal);

    my ($file, $contents) = @_;

    $$cfg{sudo} = 0 if isRoot();

    my $escFile = shell_quote $file;

    if($opts->{putCommand}){
      my $hereDoc = hereDoc $contents;
      my $cmd = "( cat $hereDoc )";

      if($$cfg{sudo}){
          $cmd .= " | sudo tee $escFile >/dev/null";
      }else{
          $cmd .= " > $escFile";
      }
      print "$cmd\n";
    }

    return if not $opts->{runCommand};

    my $cmd = $$cfg{sudo} ?
      ["|-", "sudo tee $escFile >/dev/null"] : [">", $file];

    withOpenHandle $cmd, $$cfg{fatal}, sub($){
        my $fh = shift;
        print $fh $contents;
        close $fh;
    };
}
sub writeFile     ($$) { writeFileProto {sudo => 0, fatal => 1}, @_ }
sub tryWriteFile  ($$) { writeFileProto {sudo => 0, fatal => 0}, @_ }
sub writeFileSudo ($$) { writeFileProto {sudo => 1, fatal => 1}, @_ }

sub readFileProto($@) {
    my $cfg = shift;
    assertDef $cfg, qw(sudo fatal);

    my ($file) = @_;

    $$cfg{sudo} = 0 if isRoot();

    my $escFile = shell_quote $file;

    my $cmd = $$cfg{sudo} ? ["-|", "sudo cat $escFile"] : ["<", $file];

    my @lines = withOpenHandle $cmd, $$cfg{fatal}, sub($){
        my $fh = shift;
        my @lines = <$fh>;
        close $fh;
        return @lines;
    };
    return wantarray ? @lines : join '', @lines;
}
sub readFile     ($) { readFileProto {sudo => 0, fatal => 1}, @_ }
sub tryReadFile  ($) { readFileProto {sudo => 0, fatal => 0}, @_ }
sub readFileSudo ($) { readFileProto {sudo => 1, fatal => 1}, @_ }

sub replaceLine($$$) {
    my ($s, $startRegex, $lineReplacement) = @_;
    chomp $lineReplacement;
    if($s =~ s/(^|\n+)(# ?)?$startRegex.*/$1$lineReplacement/){
        $_[0] = $s; #update in place
        return 1;
    }
    return 0;
}

sub replaceOrAddLine($$$) {
    my ($s, $startRegex, $lineReplacement) = @_;
    chomp $lineReplacement;
    if(not replaceLine $s, $startRegex, $lineReplacement){
      if(length $s > 0 and $s !~ /\n$/){
        $s .= "\n";
      }
      my $trailingEmptyLines = "";
      if($s =~ s/\n(\n*)$/\n/){
        $trailingEmptyLines = $1;
      }
      $s .= "$lineReplacement\n$trailingEmptyLines";
    }
    $_[0] = $s;
}

sub editFile($$;$) {
    my ($name, $patchname, $edit);
    ($name, $edit) = @_             if @_ == 2;
    ($name, $patchname, $edit) = @_ if @_ == 3;

    my @patchcmd = ("patch", "-fr", "-", "$name");
    my $patchfile = "$name.$patchname.patch" if defined $patchname;
    my @revcmd = (@patchcmd, $patchfile, "--reverse");

    my $escpatchcmd = join ' ', shell_quote(@patchcmd);
    my $escrevcmd   = join ' ', shell_quote(@revcmd);

    my $read;
    if (defined $patchfile and -f $patchfile) {
        if(system("$escrevcmd --dry-run >/dev/null 2>&1") != 0) {
            run @revcmd, "--dry-run";
        }

        open my $fh, "-|", @revcmd, "-s", "-o", "-";
        local $/;
        $read = <$fh>;
        close $fh;
    } else {
        $read = readFile $name;
    }

    my $tmp = $read;
    my $write = &$edit($tmp);
    unless(defined $write) {
        my $msg = shell_quote $name;
        $msg .= " " . shell_quote $patchname if defined $patchname;
        deathWithDishonor "ERROR: edit file $msg";
    }

    if($write eq $read) {
        if(defined $patchfile and -f $patchfile) {
            run @revcmd;
            run "rm", $patchfile;
        }
        return;
    }

    my $oldpatch = "";
    if (defined $patchfile and -f $patchfile) {
        $oldpatch = readFile $patchfile;
    }

    my $newpatch;
    my $pid = open my $in, "-|";
    if(not $pid) {
        open(STDERR, ">&STDOUT");

        my ($fh, $tmp) = tempfile;
        print $fh $read;
        close $fh;

        open my $out, "|-", "diff", $tmp, "-";
        print $out $write;
        close $out;
        system "rm", $tmp;
        exit;
    } else {
        local $/;
        $newpatch = <$in>;
        close $in;
    }

    if($newpatch ne $oldpatch) {
        if(defined $patchfile) {
            run @revcmd if -f $patchfile;
            writeFile $patchfile, $newpatch;
            run @patchcmd, $patchfile;
        } else {
            chomp $newpatch;
            my $hereDoc = hereDoc $newpatch;
            my $cmd = "$escpatchcmd - $hereDoc";
            shell $cmd;
        }
    }
}

sub editSimpleConf($$$) {
    my ($name, $patchname, $config) = @_;
    editFile $name, $patchname, sub {
        my $cnts = shift;
        for my $key(keys %$config){
            replaceOrAddLine $cnts, $key, "$key=$$config{$key}";
        }
        $cnts
    };
}

sub editIni($$$) {
    my ($name, $patchname, $sectionConfig) = @_;
    editFile $name, $patchname, sub {
        my $cnts = shift;

        my $curSectionName = undef;
        my $curSectionLines = undef;
        my @sectionNames;
        my %sectionLines;

        #parse out existing sections
        for my $line(split /^/, $cnts){
          if($line =~ /^\s*\[(.+)\]\s*$/){
            $curSectionName = $1;
            $curSectionLines = undef;
          }
          if(not defined $curSectionLines){
            $curSectionName = "" if not defined $curSectionName;
            $curSectionLines = [];
            if(defined $sectionLines{$curSectionName}){
              die "duplicate INI section name: \"$curSectionName\"\n";
            }
            $sectionLines{$curSectionName} = $curSectionLines;
            push @sectionNames, $curSectionName;
          }
          push @{$curSectionLines}, $line;
        }

        #add new empty sections to the parsed sections
        for my $cfgSectionName(sort keys %$sectionConfig){
          if(not defined $sectionLines{$cfgSectionName}){
            push @sectionNames, $cfgSectionName;
            my $cfgSectionLines = [];
            push @$cfgSectionLines, "[$cfgSectionName]\n";
            push @$cfgSectionLines, "\n";
            $sectionLines{$cfgSectionName} = $cfgSectionLines;
          }
        }

        #modify the parsed sections
        $cnts = "";
        for my $sectionName(@sectionNames){
          my @lines = @{$sectionLines{$sectionName}};
          my $sectionContents = join "", @lines;
          my $cfg = $$sectionConfig{$sectionName};
          if(defined $cfg){
            for my $key(sort keys %$cfg){
              my $val = $$cfg{$key};
              replaceOrAddLine $sectionContents, $key, "$key=$val";
            }
          }
          $cnts .= $sectionContents;
        }
        return $cnts;
    };
}

sub isRoot(){
    return `whoami` eq "root\n";
}

sub getRoot(@) {
    if(not isRoot()) {
        print "## rerunning as root\n";

        my $cmd = "if [ `whoami` != \"root\" ]; then exec sudo $0 @_; fi";

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        exec "sudo", $0, @_ or deathWithDishonor "failed to sudo";
    }
}

sub getRootSu(@) {
    if(not isRoot()) {
        print "## rerunning as root\n";

        my $user = getUsername();
        my $innercmd = join ' ', "SUDO_USER=$user", (shell_quote $0, @_);
        print "$innercmd\n";
        my $cmd = ""
          . "if [ `whoami` != \"root\" ]; then "
          .   "exec su -c " . (shell_quote $innercmd) . " ; "
          . "fi"
          ;

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        exec "su", "-c", $innercmd or deathWithDishonor "failed to su";
    }
}

sub guessBackupDir() {
    my $user = getUsername;
    my @dirs = sort { (stat($b))[9] <=> (stat($a))[9] }
               grep { -d $_}
               map {"/media/$_/$user"}
               split "\n", `ls -1 /media`;
    $dirs[0]
}

sub readConf($) {
    my ($file) = @_;

    my @lines = readFile($file);
    chomp @lines;
    @lines
}

sub readConfDir($) {
    my ($dir) = @_;

    my @filenames = split "\n", `ls -A1 $dir`;

    my %confs = ();
    for my $name (@filenames) {
        my @lines = readFile "$dir/$name";
        chomp @lines;
        $confs{$name} = \@lines;
    }
    %confs
}

sub installFromDir($;$$) {
    my ($dir, $gitUrl, $cmd) = (@_, undef, undef);
    if(not -d $dir and defined $gitUrl){
        runUser "mkdir", "-p", $dir;
        cd $dir;
        runUser "git", "clone", $gitUrl, ".";
    }
    cd $dir;
    tryrunUser qw(git pull) if -d ".git";

    if(defined $cmd){
      shell $cmd;
    }else{
      my @ls = split "\n", `ls -1`;
      if(grep {/\.cabal$/} @ls) {
          runUser "cabal", "install";
      } elsif(system("make -n all >/dev/null 2>&1") == 0) {
          runUser "make", "-j", "all";
          shell "sudo make install";
      } elsif(system("make -n >/dev/null 2>&1") == 0) {
          runUser "make", "-j";
          shell "sudo make install";
      } elsif(grep {/^install/} @ls) {
          shell "./install*";
      } else {
          deathWithDishonor "### no install file in $dir";
      }
    }
}

sub aptSrcInstall($$) {
    my ($package, $whichdeb) = @_;
    runAptGet "-y", "build-dep", $package;
    my $srcCache = getSrcCache();
    my $pkgSrcDir = "$srcCache/.src-cache/$package";
    shell "mkdir -p $pkgSrcDir" unless -d $pkgSrcDir;
    cd $pkgSrcDir;
    runAptGet "-b", "source", $package;
    for my $file (split "\n", `ls -1`) {
        if($file =~ /\.deb$/ && $file =~ /$whichdeb/) {
            shell "sudo dpkg -i $file";
        }
    }
}

sub removeSrcCache($) {
    my ($name) = (@_);
    my $srcCache = getSrcCache();
    run "rm", "-rf", "$srcCache/$name";
}

sub installFromGit($;$) {
    my ($gitUrl, $cmd) = (@_, undef);
    my $name = extractNameFromGitUrl $gitUrl;
    my $srcCache = getSrcCache();
    installFromDir "$srcCache/$name", $gitUrl, $cmd;
}

sub removeGitSrcCache($) {
    my ($gitUrl) = (@_);
    my $name = extractNameFromGitUrl $gitUrl;
    removeSrcCache $name;
}

sub extractNameFromGitUrl($){
    my ($gitUrl) = (@_);
    my $name;
    if($gitUrl =~ /(?:^|\/)   ([a-zA-Z0-9_\-]+)   (?:\.git)?$/x){
      $name = $1;
    }else{
      die "could not parse repo name from last element of git URL:\n$gitUrl\n";
    }
    return $name;
}

1;
