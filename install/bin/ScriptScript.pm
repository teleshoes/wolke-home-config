package ScriptScript;
use warnings;
use strict;
use File::Basename qw(basename dirname);
use File::Spec qw(abs2rel);
use File::Temp 'tempfile';
use Time::HiRes qw(sleep time);
require Exporter;

my $MODULE_AVAIL = {
  'IO::Pty'              => defined eval{require IO::Pty},
  'IPC::Run'             => defined eval{require IPC::Run},
  'String::ShellQuote'   => defined eval{require String::ShellQuote},
};

our @ISA = qw(Exporter);
our @EXPORT_OK = qw();
our @EXPORT = qw( getScriptNames getSubNames
                  getInstallNames getInstallScriptNames getInstallSrcNames getInstallPipNames
                  run tryrun
                  runUser tryrunUser wrapUserCommand
                  runAptGet tryrunAptGet
                  proc procLines procUser tryproc
                  runScript
                  getHome getInstallPath getSrcCache
                  getMachineType
                  getResconfigScale
                  symlinkFile symlinkFileRel symlinkFileSudo symlinkFileRelSudo
                  which
                  globOne
                  writeFile tryWriteFile writeFileSudo
                  readFile tryReadFile readFileSudo
                  replaceLine replaceOrAddLine
                  editFile editSimpleConf editIni editFileLines
                  getRoot getRootSu
                  getUsername
                  guessBackupDir
                  readConf readConfDir
                  installFromDir removeSrcCache
                  installFromGit removeGitSrcCache extractNameFromGitUrl
                  shellQuote
                  md5sum
                  nowMillis
                );

sub getScriptNames();
sub getSubNames();
sub assertDef($@);
sub runProto($@);
sub runProtoIPC($$@);
sub runProtoNoIPC($$@);
sub run(@);
sub tryrun(@);
sub runUser(@);
sub tryrunUser(@);
sub wrapUserCommand(@);
sub proc(@);
sub procLines(@);
sub procUser(@);
sub tryproc(@);
sub readProcessLines(@);
sub tryreadProcessLines(@);
sub runScript($@);
sub getUsername();
sub getInstallPath($);
sub which($);
sub symlinkFileProto($$;$$);
sub symlinkFile($$);
sub symlinkFileSudo($$);
sub symlinkFileRel($$);
sub symlinkFileRelSudo($$);
sub globOne($);
sub writeFileProto($$$);
sub writeFile($$);
sub tryWriteFile($$);
sub writeFileSudo($$);
sub readFileProto($$);
sub readFile($);
sub tryReadFile($);
sub readFileSudo($);
sub replaceLine($$$);
sub replaceOrAddLine($$$);
sub editFile($$$);
sub editSimpleConf($$$);
sub editIni($$$);
sub getRoot(@);
sub getRootSu(@);
sub guessBackupDir();
sub readConf($);
sub readConfDir($);
sub installFromDir($;$$);
sub removeSrcCache($);
sub installFromGit($;$);
sub removeGitSrcCache($);
sub extractNameFromGitUrl($);
sub shellQuote(@);
sub md5sum($);

my $FILES_TO_DELETE = {};

$SIG{INT} = sub{
  system "rm", "-f", $_ foreach keys %$FILES_TO_DELETE;
  exit 130;
};

my @SHELL_METACHAR_LIST = (
  "|", "&", ";", "<", ">", "(", ")", "\$", "`",
  "\\", "'", "\"", "\n", " ", "\t",
  "=", "*", "#",
);
my $SHELL_METACHAR_REGEX = "[" . join("", @SHELL_METACHAR_LIST) . "]";

my $SIMULATE = 0;

sub getScriptNames(){
  my $bin = getInstallPath "bin";
  my @scripts = grep {-f $_} glob "$bin/*";
  s/$bin\/// foreach @scripts;
  @scripts = grep {/^[a-zA-Z0-9_\-]+$/} @scripts;
  return \@scripts;
}
sub getSubNames(){
  return \@EXPORT;
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
  return [grep {defined $_} map {s/^install-(.+)//; $1} @{getScriptNames()}];
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

sub assertDef($@){
  my ($hash, @targetKeys) = @_;
  my @keys = keys %$hash;
  my $targetHash = {map {$_ => 1} @targetKeys};
  for my $key(@keys){
    if(not defined $$targetHash{$key}){
      die "ERROR: extra arg '$key' (expected: @targetKeys, actual: @keys)\n";
    }
  }
  for my $key(@targetKeys){
    if(not defined $$hash{$key}){
      die "ERROR: missing arg '$key' (expected: @targetKeys, actual: @keys)\n";
    }
  }
}

sub runProto($@){
  my ($cfg, @cmd) = @_;
  assertDef $cfg, qw(printCmd printOut includeErr progressBar fatal);

  if(@cmd == 1 and $cmd[0] =~ /$SHELL_METACHAR_REGEX/){
    @cmd = ("/bin/sh", "-c", "@cmd");
  }

  print "@cmd\n" if $$cfg{printCmd};

  if($SIMULATE){
    return;
  }

  my $progressBarFile = undef;
  if($$cfg{progressBar}){
    system "rm -f /tmp/progress-bar-*";
    $progressBarFile = "/tmp/progress-bar-" . nowMillis() . ".txt";
    $$FILES_TO_DELETE{$progressBarFile} = 1;
  }

  my $outputAction = sub {
    my $output = shift;
    if(defined $progressBarFile and $output =~ /(100|\d\d|\d)%/){
      open my $fh, "> $progressBarFile";
      print $fh "$1\n";
      close $fh;
    }

    if($$cfg{printOut}){
      print $output;
      STDOUT->flush();
    }
  };

  my $result;
  if($$MODULE_AVAIL{'IPC::Run'} and $$MODULE_AVAIL{'IO::Pty'}){
    $result = runProtoIPC($cfg, $outputAction, @cmd);
  }else{
    $result = runProtoNoIPC($cfg, $outputAction, @cmd);
  }

  if($$cfg{progressBar}){
    system "rm", "-f", $progressBarFile;
    delete $$FILES_TO_DELETE{$progressBarFile};
  }

  if(not $$result{success}){
    if($$cfg{fatal}){
      die "ERROR: cmd '@cmd' failed\n";
    }else{
      print STDERR "WARNING: cmd '@cmd' failed\n";
    }
  }

  return $$result{success};
}
sub runProtoIPC($$@) {
  my ($cfg, $outputAction, @cmd) = @_;

  my $pty = new IO::Pty();
  my $slave = $pty->slave;
  $pty->blocking(0);
  $slave->blocking(0);

  my $pipeOp = $$cfg{includeErr} ? "&>" : ">";

  my $result = {
    success   => undef,
    exitCode  => undef,
  };

  my $h = IPC::Run::harness(\@cmd, $pipeOp, $slave);
  if($$cfg{fatal}){
    $h->start;
  }else{
    $h = eval {$h->start};
    if(not defined $h){
      $$result{success} = 0;
      return $result;
    }
  }

  my $out;
  while($h->pumpable() or $out = <$pty>){
    if($h->pumpable()){
      eval { $h->pump_nb }; #eval because pumpable doesn't actually guarantee no error
    }
    if(not defined $out or length $out == 0){
      $out = <$pty>;
    }
    if(defined $out and length $out > 0){
      &$outputAction($out);
      $out = undef;
    }else{
      sleep 0.01; #small delay to decrease busy-wait on input
    }

    <$slave>;
  }

  IPC::Run::finish $h;
  close $pty;
  close $slave;

  my $exitCode = $h->result;
  $$result{success} = $exitCode == 0 ? 1 : 0;
  $$result{exitCode} = $exitCode;

  return $result;
}
sub runProtoNoIPC($$@) {
  my ($cfg, $outputAction, @cmd) = @_;

  my $result = {
    success   => undef,
    exitCode  => undef,
  };

  my $pid = open my $fh, "-|";
  if(not $pid) {
    if($$cfg{includeErr}){
      open(STDERR, ">&STDOUT");
    }
    exec @cmd or die "ERROR: cmd '@cmd' failed\n";
  } else {
    while(my $line = <$fh>) {
      chomp $line;
      $line = "$line\n";
      &$outputAction($line);
    }
    close $fh;

    my $exitCode = $?;
    $$result{success} = $exitCode == 0 ? 1 : 0;
    $$result{exitCode} = $exitCode;

    return $result;
  }
}

sub run(@){
  return runProto {printCmd => 1, printOut => 1, includeErr => 1, progressBar => 1, fatal => 1}, @_;
}
sub tryrun(@){
  return runProto {printCmd => 1, printOut => 1, includeErr => 1, progressBar => 1, fatal => 0}, @_;
}
sub runUser(@){
  return run wrapUserCommand(@_);
}
sub tryrunUser(@){
  return tryrun wrapUserCommand(@_);
}

sub runAptGet(@){
  my @cmd = isRoot() ? ("apt-get", @_) : ("sudo", "apt-get", @_);
  run @cmd;
}
sub tryrunAptGet(@){
  my @cmd = isRoot() ? ("apt-get", @_) : ("sudo", "apt-get", @_);
  tryrun @cmd;
}

sub wrapUserCommand(@) {
  return isRoot() ? ("su", getUsername(), "-c", (join ' ', shellQuote @_)) : @_;
}

sub proc(@) {
  my @lines = readProcessLines @_;
  my $out = join '', @lines;
  chomp $out;
  return $out;
}
sub procLines(@) {
  my @lines = readProcessLines @_;
  chomp foreach @lines;
  return @lines;
}
sub procUser(@) {
  return proc(wrapUserCommand(@_));
}
sub tryproc(@) {
  my @lines = tryreadProcessLines @_;
  my $out = join '', @lines;
  chomp $out;
  return $out;
}

sub readProcessLines(@){
  my @cmd = @_;
  open PROC_FH, "-|", @cmd or die "could not run @cmd\n";
  my @lines = <PROC_FH>;
  close PROC_FH;
  die "error running cmd: @cmd\n" if $? != 0;
  chomp foreach @lines;
  return @lines;
}
sub tryreadProcessLines(@){
  my @cmd = @_;
  my @lines;
  if(open PROC_FH, "-|", @cmd){
    @lines = <PROC_FH>;
    close PROC_FH;
  }
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
    die "ERROR: USER or SUDO_USER must be set, and cannot be root";
  }
  $user
}

sub getMachineType() {
  my $machineType = tryReadFile(getHome() . "/machine-type");
  $machineType = "" if not defined $machineType;
  chomp $machineType;
  my %machineTypes = map {basename($_) => 1} glob(getHome() . "/machine-types/*");
  if(defined $machineTypes{$machineType}){
    return $machineType;
  }else{
    return undef;
  }
}

sub getResconfigScale(){
  my $defaultWidthPx = 1920;
  my $resconfigWidthPx = `resconfig --width-px 2>/dev/null`;
  chomp $resconfigWidthPx;
  if($resconfigWidthPx =~ /^\d+$/){
    return $resconfigWidthPx / $defaultWidthPx;
  }else{
    return 1.0;
  }
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

sub symlinkFileProto($$;$$) {
  my ($srcPath, $destFile, $convertToRelPath, $useSudo) = @_;
  $srcPath = File::Spec->abs2rel($srcPath, dirname $destFile) if $convertToRelPath;
  my @sudo = $useSudo ? ("sudo") : ();

  if(-l $destFile){
    my $oldPath = readlink $destFile;
    if($oldPath eq $srcPath){
      print "  symlink unchanged $srcPath => $destFile\n";
      return;
    }else{
      run @sudo, "rm", $destFile;
      print "  symlink $destFile: $oldPath => $srcPath\n";
    }
  }elsif(-d $destFile){
    run @sudo, "rmdir", $destFile;
    print "  dir=>symlink: $destFile\n";
  }
  die "Error creating symlink file $destFile\n" if -e $destFile;
  run @sudo, "ln", "-s", $srcPath, $destFile;
  die "Error creating symlink file $destFile\n" if not -e $destFile;
}
sub symlinkFile($$) {
  symlinkFileProto($_[0], $_[1], 0, 0);
}
sub symlinkFileRel($$) {
  symlinkFileProto($_[0], $_[1], 1, 0);
}
sub symlinkFileSudo($$) {
  symlinkFileProto($_[0], $_[1], 0, 1);
}
sub symlinkFileRelSudo($$) {
  symlinkFileProto($_[0], $_[1], 1, 1);
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

sub writeFileProto($$$) {
  my ($cfg, $file, $contents) = @_;
  assertDef $cfg, qw(sudo fatal);

  if($SIMULATE){
    print "## write $file:\n$contents\n";
    return;
  }

  my $fh;
  my $status;
  if($$cfg{sudo} and not isRoot()){
    $status = open $fh, "|-", "sudo", "tee", $file;
  }else{
    $status = open $fh, ">", $file;
  }
  if(not $status){
    if($$cfg{fatal}){
      die "ERROR: could not write $file\n$!\n";
    }else{
      print STDERR "WARNING: could not write $file\n$!\n";
      close $fh;
      return;
    }
  }
  print $fh $contents;
  close $fh;
}
sub writeFile     ($$) { writeFileProto({sudo => 0, fatal => 1}, $_[0], $_[1]); }
sub tryWriteFile  ($$) { writeFileProto({sudo => 0, fatal => 0}, $_[0], $_[1]); }
sub writeFileSudo ($$) { writeFileProto({sudo => 1, fatal => 1}, $_[0], $_[1]); }

sub readFileProto($$) {
  my ($cfg, $file) = @_;
  assertDef $cfg, qw(sudo fatal);

  my $fh;
  my $status;
  if($$cfg{sudo} and not isRoot()){
    $status = open $fh, "-|", "sudo", "cat", $file;
  }else{
    $status = open $fh, "<", $file;
  }
  if(not $status){
    if($$cfg{fatal}){
      die "ERROR: could not read $file\n$!\n";
    }else{
      print STDERR "WARNING: could not read $file\n$!\n";
      close $fh;
      return;
    }
  }
  my @lines = <$fh>;
  close $fh;

  return wantarray ? @lines : join '', @lines;
}
sub readFile     ($) { readFileProto({sudo => 0, fatal => 1}, $_[0]); }
sub tryReadFile  ($) { readFileProto({sudo => 0, fatal => 0}, $_[0]); }
sub readFileSudo ($) { readFileProto({sudo => 1, fatal => 1}, $_[0]); }

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

sub editFile($$$) {
  my ($file, $backupName, $editSub) = @_;

  #dereference non-broken symlink to file
  my $symlinkLevel = 0;
  while(-f $file and -l $file){
    die "ERROR: symlink level too high\n" if $symlinkLevel > 10;
    $file = readlink $file;
    $symlinkLevel++;
  }

  if(not -f $file){
    die "ERROR: $file does not exist or is not a file\n";
  }elsif(-l $file){
    die "ERROR: $file is a symlink after dereferencing, not editing\n";
  }

  my $oldContents = readFile $file;
  my $newContents = &$editSub($oldContents);

  if($oldContents eq $newContents){
    print "file unchanged: $file\n";
    return 0;
  }else{
    if($SIMULATE){
      print "# file would be changed: $file\n$newContents\n";
    }else{
      my $secondPassContents = &$editSub($newContents);
      if($newContents ne $secondPassContents){
        print STDERR "WARNING: non-idempotent edit for file $file\n";
      }

      my $bakFile = join ".", grep {defined $_} ($file, "bak", $backupName, nowMillis());
      run "cp", "-a", $file, $bakFile;
      if(not -f $bakFile){
        die "ERROR: could not create backup file $bakFile\n";
      }
      writeFile $file, $newContents;

      tryrun "diff", $bakFile, $file;
    }
    return 1;
  }
}

sub editFileLines($$$) {
  my ($file, $backupName, $editLineSub) = @_;
  editFile $file, $backupName, sub {
    my $cnts = shift;
    my @lines = split /(?<=\n)/, $cnts;
    for my $line(@lines){
      $line = &$editLineSub($line);
    }
    $cnts = join '', @lines;
    $cnts;
  };
}

sub editSimpleConf($$$) {
  my ($file, $backupName, $config) = @_;
  editFile $file, $backupName, sub {
    my $cnts = shift;
    for my $key(sort keys %$config){
      replaceOrAddLine $cnts, $key, "$key=$$config{$key}";
    }
    $cnts;
  };
}

sub editIni($$$) {
  my ($file, $backupName, $sectionConfig) = @_;
  editFile $file, $backupName, sub {
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
    $cnts;
  };
}

sub isRoot(){
  return `whoami` eq "root\n";
}

sub getRoot(@) {
  if(not isRoot()) {
    print "## rerunning as root\n";
    if($SIMULATE){
      return;
    }else{
      exec "sudo", $0, @_ or die "ERROR: exec sudo failed\n";
    }
  }
}

sub getRootSu(@) {
  if(not isRoot()) {
    print "## rerunning as root with su\n";

    my $user = getUsername();
    my $cmd = shellQuote($0, @_);
    $cmd = "SUDO_USER=$user $cmd";

    if($SIMULATE){
      return;
    }else{
      exec "su", "-c", $cmd or die "ERROR: exec su failed\n";
    }
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
    runUser "git", "-C", $dir, "clone", $gitUrl, ".";
  }
  if(-d "$dir/.git"){
    tryrunUser "git", "-C", $dir, "pull";
  }

  if(not defined $cmd or $cmd eq ""){
    my @files = grep {-e $_} glob "$dir/*";
    my @cabalFiles = grep {/^$dir\/.*\.cabal$/} @files;
    my @installCmds = grep {-x $_ and -f $_ and $_ =~ /^$dir\/install/} @files;
    if(@cabalFiles > 0) {
      $cmd = "cabal install -j";
    } elsif(system("make -C '$dir' -n all >/dev/null 2>&1") == 0) {
      $cmd = "make -j all && sudo make install";
    } elsif(system("make -C '$dir' -n >/dev/null 2>&1") == 0) {
      $cmd = "make -j && sudo make install";
    } elsif(@installCmds == 1) {
      $cmd = $installCmds[0];
    } else {
      die "### no install file in $dir";
    }
  }

  runUser "cd '$dir' && $cmd";
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
  if($gitUrl =~ /(?:^|\/)   ([a-zA-Z0-9_\.\-]+)   (?:\.git)?$/x){
    $name = $1;
  }else{
    die "could not parse repo name from last element of git URL:\n$gitUrl\n";
  }
  return $name;
}

sub shellQuote(@){
  if($$MODULE_AVAIL{'String::ShellQuote'}){
    return String::ShellQuote::shell_quote(@_);
  }else{
    return join " ", map {
      my $str = $_;
      $str =~ s/\\/\\\\/g;
      $str =~ s/'/'\\''/g;
      $str = "'$str'";
      $str;
    } @_;
  }
}

sub md5sum($){
  my ($file) = @_;
  return undef if not -f $file;
  my $md5 = tryproc "md5sum", $file;
  if($md5 =~ /^([0-9a-f]{32})\s+($file)$/){
    return $1;
  }else{
    return undef;
  }
}

sub nowMillis(){
  return int(time * 1000.0 + 0.5);
}

1;
