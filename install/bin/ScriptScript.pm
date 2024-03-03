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
  'B::Deparse'           => defined eval{require B::Deparse},
};

our @ISA = qw(Exporter);
our @EXPORT_OK = qw();
our @EXPORT = qw(
  getScriptNames getSubNames
  getInstallNames getInstallScriptNames getInstallSrcNames getInstallPipNames
  run tryrun runSudo tryrunSudo tryrunSilent runWithStderr runNoPty
  runUser tryrunUser runUserNoPty
  proc procChomp procUser tryproc
  runAptGet tryrunAptGet
  runScript
  getUsername
  getMachineType getResconfigScale
  getHome getInstallPath getSrcCache
  symlinkFile symlinkFileRel symlinkFileSudo symlinkFileRelSudo
  globAll globFiles globDirs
  globFilesBasename
  globOne
  writeFile tryWriteFile writeFileSudo
  readFile tryReadFile readFileSudo readFileChomp
  replaceLine replaceOrAddLine
  editFile editFileLines editFileSimpleConf editFileIni
  getRoot getRootSu
  readConfDir
  installFromGit removeSrcCache removeGitSrcCache extractNameFromGitUrl
  shellQuote
  versionCmp
  joinLines
  md5sum
  nowMillis
);

my $WANTARRAY_CONTEXT_VOID = "void";
my $WANTARRAY_CONTEXT_LIST = "list";
my $WANTARRAY_CONTEXT_SCALAR = "scalar";

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

my $FORCE_SIMULATE = 0; #or env var SS_SIMULATE=true

sub getScriptNames();
sub getSubNames();
sub getInstallNames();
sub getInstallScriptNames();
sub getInstallSrcNames();
sub getInstallPipNames();
sub assertDef($@);
sub wantarrayToContext($);
sub runProto($@);
sub runCommandPty($$@);
sub runCommandNoPty($$@);
sub run(@);
sub tryrun(@);
sub runSudo(@);
sub tryrunSudo(@);
sub runNoPty(@);
sub tryrunSilent(@);
sub runUser(@);
sub tryrunUser(@);
sub runUserNoPty(@);
sub proc(@);
sub procChomp(@);
sub procUser(@);
sub tryproc(@);
sub runAptGet(@);
sub tryrunAptGet(@);
sub runScript($@);
sub getUsername();
sub getMachineType();
sub getResconfigScale();
sub getHome();
sub getInstallPath($);
sub getSrcCache();
sub symlinkFileProto($$$);
sub symlinkFile($$);
sub symlinkFileRel($$);
sub symlinkFileSudo($$);
sub symlinkFileRelSudo($$);
sub globAll($);
sub globFiles($);
sub globDirs($);
sub globFilesBasename($);
sub globOne($);
sub writeFileProto($$$);
sub writeFile($$);
sub tryWriteFile($$);
sub writeFileSudo($$);
sub readFileProto($$);
sub readFile($);
sub tryReadFile($);
sub readFileSudo($);
sub readFileChomp($);
sub replaceLine($$$);
sub replaceOrAddLine($$$);
sub editFile($$);
sub editFileLines($$);
sub editFileSimpleConf($$);
sub editFileIni($$);
sub isSimulate();
sub isRoot();
sub getRoot(@);
sub getRootSu(@);
sub readConfDir($);
sub installFromGit($$);
sub removeSrcCache($);
sub removeGitSrcCache($);
sub extractNameFromGitUrl($);
sub shellQuote(@);
sub md5sum($);
sub nowMillis();

sub getScriptNames(){
  return globFilesBasename(getInstallPath("bin/*"));
}
sub getSubNames(){
  return @EXPORT;
}
sub getInstallNames(){
  return (
    getInstallScriptNames(),
    getInstallSrcNames(),
    getInstallPipNames(),
  );
}
sub getInstallScriptNames(){
  return grep {defined $_} map {/^install-(.+)/ ? $1 : undef} getScriptNames();
}
sub getInstallSrcNames(){
  return (procChomp(getInstallPath("bin/install-src"), "--list"));
}
sub getInstallPipNames(){
  return (procChomp(getInstallPath("bin/install-pip-packages"), "--list"));
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

sub wantarrayToContext($){
  my ($wantarrayValue) = @_;
  if(not defined $wantarrayValue){
    return $WANTARRAY_CONTEXT_VOID;
  }elsif($wantarrayValue){
    return $WANTARRAY_CONTEXT_LIST;
  }else{
    return $WANTARRAY_CONTEXT_SCALAR;
  }
}

sub runProto($@){
  my ($cfg, @cmd) = @_;
  $cfg = {
    pty           => 1,
    chomp         => 0,
    returnOut     => 0,
    wrapSudoCmd   => 0,
    wrapUserCmd   => 0,
    printCmd      => 1,
    printOut      => 1,
    stderrToOut   => 1,
    progressBar   => 1,
    fatal         => 1,
    %$cfg,
  };
  assertDef $cfg, qw(
    pty chomp returnOut wrapSudoCmd wrapUserCmd printCmd printOut stderrToOut progressBar fatal);

  if(@cmd == 1 and $cmd[0] =~ /$SHELL_METACHAR_REGEX/){
    @cmd = ("/bin/sh", "-c", "@cmd");
  }

  if($$cfg{wrapSudoCmd} and not isRoot()){
    @cmd = ("sudo", @cmd);
  }

  if($$cfg{wrapUserCmd} and isRoot()){
    @cmd = ("sudo", "-u", getUsername(), @cmd);
  }

  print "@cmd\n" if $$cfg{printCmd};

  if(isSimulate()){
    return;
  }

  my $progressBarFile = undef;
  if($$cfg{progressBar}){
    my @progBarFiles = globFiles "/tmp/progress-bar-*";
    system "rm", @progBarFiles if @progBarFiles > 0;

    $progressBarFile = "/tmp/progress-bar-" . nowMillis() . ".txt";
    $$FILES_TO_DELETE{$progressBarFile} = 1;
  }

  my $wantarrayContext = wantarrayToContext(wantarray);

  my $resultOutput = "";

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

    if($$cfg{returnOut} and $wantarrayContext ne $WANTARRAY_CONTEXT_VOID){
      $resultOutput .= $output;
    }
  };

  my $result;
  if($$cfg{pty} and $$MODULE_AVAIL{'IPC::Run'} and $$MODULE_AVAIL{'IO::Pty'}){
    $result = runCommandPty($$cfg{stderrToOut}, $outputAction, @cmd);
  }else{
    $result = runCommandNoPty($$cfg{stderrToOut}, $outputAction, @cmd);
  }

  if($$cfg{progressBar}){
    system "rm", "-f", $progressBarFile;
    delete $$FILES_TO_DELETE{$progressBarFile};
  }

  if(not $$result{success}){
    if($$cfg{fatal}){
      die "ERROR: cmd '@cmd' failed\n$$result{exception}\n";
    }elsif(length $$result{exception} > 0){
      #warn only if $! or $@ was set
      print STDERR "WARNING: cmd '@cmd' failed\n$$result{exception}\n";
    }
  }

  if(not $$cfg{returnOut}){
    return $$result{success};
  }elsif($wantarrayContext eq $WANTARRAY_CONTEXT_VOID){
    return;
  }elsif($wantarrayContext eq $WANTARRAY_CONTEXT_LIST){
    my @lines = split /(?<=\n)/, $resultOutput;
    if($$cfg{chomp}){
      s/[\r\n]+$// foreach @lines;
    }
    return @lines;
  }elsif($wantarrayContext eq $WANTARRAY_CONTEXT_SCALAR){
    my $output = $resultOutput;
    if($$cfg{chomp}){
      $output =~ s/[\r\n]+$//;
    }
    return $output;
  }else{
    die "ERROR: could not parse wantarray context\n";
  }
}
sub runCommandPty($$@){
  my ($stderrToOut, $outputAction, @cmd) = @_;

  my $pty = new IO::Pty();
  my $slave = $pty->slave;
  $pty->blocking(0);
  $slave->blocking(0);

  my $pipeOp = $stderrToOut ? "&>" : ">";

  my $result = {
    success        => undef,
    exception      => "",
    exitStatusFull => undef,
    exitCode       => undef,
    exitSignalNum  => undef,
    exitDumpedCore => undef,
  };

  my $h = IPC::Run::harness(\@cmd, $pipeOp, $slave);
  $h = eval {$h->start};
  if(not defined $h){
    $$result{success} = 0;
    $$result{exception} = $@;
    return $result;
  }

  my $out;
  while($h->pumpable() or $out = <$pty>){
    if($h->pumpable()){
      eval { $h->pump_nb }; #eval because pumpable doesn't actually guarantee no error
    }
    if(not defined $out or length $out == 0){
      $pty->blocking(0);
      $out = <$pty>;
    }
    if(defined $out and length $out > 0){
      &$outputAction($out);
      $out = undef;
    }else{
      sleep 0.01; #small delay to decrease busy-wait on input
    }

    $slave->blocking(0);
    <$slave>;
  }

  IPC::Run::finish $h;
  close $pty;
  close $slave;

  $$result{exitStatusFull} = $h->full_result();

  $$result{success} = $$result{exitStatusFull} == 0 ? 1 : 0;
  $$result{exitCode} = $$result{exitStatusFull} >> 8;
  $$result{exitSignalNum} = $$result{exitStatusFull} & 127;
  $$result{exitDumpedCore} = $$result{exitStatusFull} & 128;

  return $result;
}
sub runCommandNoPty($$@){
  my ($stderrToOut, $outputAction, @cmd) = @_;

  my $result = {
    success        => undef,
    exception      => "",
    exitStatusFull => undef,
    exitCode       => undef,
    exitSignalNum  => undef,
    exitDumpedCore => undef,
  };

  my $pid = open my $fh, "-|";
  if(not $pid){
    if($stderrToOut){
      open(STDERR, ">&STDOUT");
    }
    exec @cmd or die "ERROR: cmd '@cmd' failed\n$!\n";
  }else{
    while(my $line = <$fh>){
      &$outputAction($line);
    }
    close $fh;

    $$result{exitStatusFull} = $?;
    $$result{exception} = $! if $$result{exitStatusFull} != 0 and defined $!;

    $$result{success} = $$result{exitStatusFull} == 0 ? 1 : 0;
    $$result{exitCode} = $$result{exitStatusFull} >> 8;
    $$result{exitSignalNum} = $$result{exitStatusFull} & 127;
    $$result{exitDumpedCore} = $$result{exitStatusFull} & 128;

    return $result;
  }
}

sub run(@){
  return runProto({}, @_);
}
sub tryrun(@){
  return runProto({fatal=>0}, @_);
}
sub runSudo(@){
  return runProto({wrapSudoCmd=>1}, @_);
}
sub tryrunSudo(@){
  return runProto({wrapSudoCmd=>1, fatal=>0}, @_);
}
sub runNoPty(@){
  return runProto({pty=>0}, @_);
}
sub tryrunSilent(@){
  return runProto({fatal=>0, printOut=>0, printCmd=>0}, @_);
}
sub runUser(@){
  return runProto({wrapUserCmd=>1}, @_);
}
sub tryrunUser(@){
  return runProto({wrapUserCmd=>1, fatal=>0}, @_);
}
sub runUserNoPty(@){
  return runProto({wrapUserCmd=>1, pty=>0}, @_);
}
sub runWithStderr(@){
  return runProto({stderrToOut=>0}, @_);
}

sub proc(@){
  return runProto({returnOut=>1, printOut=>0, printCmd=>0, pty=>0}, @_);
}
sub procChomp(@){
  return runProto({returnOut=>1, printOut=>0, printCmd=>0, pty=>0, chomp=>1}, @_);
}
sub procUser(@){
  return runProto({returnOut=>1, printOut=>0, printCmd=>0, pty=>0, wrapUserCmd=>1}, @_);
}
sub tryproc(@){
  return runProto({returnOut=>1, printOut=>0, printCmd=>0, pty=>0, fatal=>0}, @_);
}

sub runAptGet(@){
  my @cmd = isRoot() ? ("apt-get", @_) : ("sudo", "apt-get", @_);
  run @cmd;
}
sub tryrunAptGet(@){
  my @cmd = isRoot() ? ("apt-get", @_) : ("sudo", "apt-get", @_);
  tryrun @cmd;
}


sub runScript($@){
  my $scriptName = shift;
  my $script = getInstallPath "bin/$scriptName";
  runWithStderr $script, @_;
}

sub getUsername(){
  my $user = $ENV{SUDO_USER} || $ENV{USER};
  if(not $user or $user eq "root"){
    die "ERROR: USER or SUDO_USER must be set, and cannot be root";
  }
  return $user;
}

sub getMachineType(){
  my $file = getHome() . "/machine-type";
  return undef if not -f $file;
  my $machineType = readFileChomp(getHome() . "/machine-type");
  my %machineTypes = globFilesBasename(getHome() . "/machine-types/*");
  if(defined $machineTypes{$machineType}){
    return $machineType;
  }else{
    return undef;
  }
}

sub getResconfigScale(){
  my $defaultWidthPx = 1920;
  my $resconfigWidthPx = procChomp "resconfig", "--width-px";
  if($resconfigWidthPx =~ /^\d+$/){
    return $resconfigWidthPx / $defaultWidthPx;
  }else{
    return 1.0;
  }
}

sub getHome(){
  my $home;
  if(not isRoot() and $ENV{HOME} =~ /^\/home/){
    $home = $ENV{HOME};
  }else{
    $home = "/home/" . getUsername();
  }
  if(not -d $home){
    die "ERROR: $home is not a dir\n";
  }
  return $home;
}

sub getInstallPath($){
  return getHome() . "/install/$_[0]";
}

sub getSrcCache(){
  return getHome() . "/.src-cache";
}

sub symlinkFileProto($$$){
  my ($cfg, $srcPath, $destFile) = @_;
  $cfg = {
    relPath => 0,
    sudo    => 0,
    %$cfg,
  };
  assertDef $cfg, qw(relPath sudo);

  #optionally relativize target to the symlink's dir
  #  e.g.:
  #    1)    ln -s /var/log/app/app.log /var/log/app-log
  #       => ln -s          app/app.log /var/log/app-log
  #    2)    ln -s /usr/lib/a.so /usr/lib/linux/a.so
  #       => ln -s       ../a.so /usr/lib/linux/a.so
  if($$cfg{relPath}){
    my $destDir = dirname $destFile;
    $srcPath = File::Spec->abs2rel($srcPath, $destDir);
  }

  my @sudo = $$cfg{sudo} ? ("sudo") : ();

  #allow replacing existing symlinks
  if(-l $destFile){
    my $oldPath = readlink $destFile;
    if($oldPath eq $srcPath){
      print "  symlink unchanged $srcPath => $destFile\n";
      return;
    }else{
      print "  symlink $destFile: $oldPath => $srcPath\n";
      if(not isSimulate()){
        run @sudo, "rm", $destFile;
      }
    }
  }

  if(isSimulate()){
    print "  symlink $srcPath => $destFile\n";
    return;
  }

  if(-e $destFile){
    die "ERROR: symlink file $destFile already exists\n";
  }

  run @sudo, "ln", "-s", $srcPath, $destFile;

  if(not -l $destFile){
    die "ERROR: symlink file $destFile does not exist after creation\n";
  }
}
sub symlinkFile($$){
  symlinkFileProto({}, $_[0], $_[1]);
}
sub symlinkFileRel($$){
  symlinkFileProto({relPath => 1}, $_[0], $_[1]);
}
sub symlinkFileSudo($$){
  symlinkFileProto({sudo => 1}, $_[0], $_[1]);
}
sub symlinkFileRelSudo($$){
  symlinkFileProto({relPath => 1, sudo => 1}, $_[0], $_[1]);
}

sub globAll($){
  return grep {-e $_} glob $_[0];
}
sub globFiles($){
  return grep {-f $_} glob $_[0];
}
sub globDirs($){
  return grep {-d $_} glob $_[0];
}

sub globFilesBasename($){
  return map {basename($_)} globFiles $_[0];
}

sub globOne($){
  my @files = globAll $_[0];
  if(@files == 1){
    return $files[0];
  }else{
    return undef;
  }
}

sub writeFileProto($$$){
  my ($cfg, $file, $contents) = @_;
  $cfg = {
    sudo  => 0,
    fatal => 1,
    %$cfg,
  };
  assertDef $cfg, qw(sudo fatal);

  if(isSimulate()){
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
sub writeFile($$){
  writeFileProto({}, $_[0], $_[1]);
}
sub tryWriteFile($$){
  writeFileProto({fatal => 0}, $_[0], $_[1]);
}
sub writeFileSudo($$){
  writeFileProto({sudo => 1}, $_[0], $_[1]);
}

sub readFileProto($$){
  my ($cfg, $file) = @_;
  $cfg = {
    sudo  => 0,
    fatal => 1,
    chomp => 0,
    %$cfg,
  };
  assertDef $cfg, qw(sudo fatal chomp);

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

  my $wantarrayContext = wantarrayToContext(wantarray);

  if($wantarrayContext eq $WANTARRAY_CONTEXT_VOID){
    return;
  }elsif($wantarrayContext eq $WANTARRAY_CONTEXT_LIST){
    if($$cfg{chomp}){
      s/[\r\n]+$// foreach @lines;
    }
    return @lines;
  }elsif($wantarrayContext eq $WANTARRAY_CONTEXT_SCALAR){
    my $content = join '', @lines;
    if($$cfg{chomp}){
      $content =~ s/[\r\n]+$//;
    }
    return $content;
  }else{
    die "ERROR: could not parse wantarray context\n";
  }
}
sub readFile($){
  readFileProto({}, $_[0]);
}
sub tryReadFile($){
  readFileProto({fatal => 0}, $_[0]);
}
sub readFileSudo($){
  readFileProto({sudo => 1}, $_[0]);
}
sub readFileChomp($){
  readFileProto({chomp => 1}, $_[0]);
}

sub replaceLine($$$){
  my ($s, $startRegex, $lineReplacement) = @_;
  chomp $lineReplacement;
  if($s =~ s/(^|\n+)(# ?)?$startRegex.*/$1$lineReplacement/){
    $_[0] = $s; #update in place
    return 1;
  }
  return 0;
}

sub replaceOrAddLine($$$){
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

sub editFile($$){
  my ($file, $editSub) = @_;

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
    if(isSimulate()){
      print "# file would be changed: $file\n$newContents\n";
    }else{
      my $secondPassContents = &$editSub($newContents);
      if($newContents ne $secondPassContents){
        print STDERR "WARNING: non-idempotent edit for file $file\n";
      }

      my $bakFile = "$file.bak." . nowMillis();
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

sub editFileLines($$){
  my ($file, $editLineSub) = @_;
  editFile $file, sub {
    my $cnts = shift;
    my @lines = split /(?<=\n)/, $cnts;
    for my $line(@lines){
      $line = &$editLineSub($line);
    }
    $cnts = join '', @lines;
    $cnts;
  };
}

sub editFileSimpleConf($$){
  my ($file, $config) = @_;
  editFile $file, sub {
    my $cnts = shift;
    for my $key(sort keys %$config){
      replaceOrAddLine $cnts, $key, "$key=$$config{$key}";
    }
    $cnts;
  };
}

sub editFileIni($$){
  my ($file, $sectionConfig) = @_;
  editFile $file, sub {
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

sub isSimulate(){
  if($FORCE_SIMULATE){
    return 1;
  }
  my $simulate = $ENV{SS_SIMULATE};
  $simulate = "" if not defined $simulate;
  if($simulate eq "" or $simulate =~ /^(0|no|false)$/i){
    return 0;
  }else{
    return 1;
  }
}

sub isRoot(){
  if(isSimulate()){
    return $ENV{USER} eq "root" ? 1 : 0;
  }else{
    return procChomp("whoami") eq "root" ? 1 : 0;
  }
}

sub getRoot(@){
  if(not isRoot()){
    print "## rerunning as root\n";
    if(isSimulate()){
      return;
    }else{
      exec "sudo", $0, @_ or die "ERROR: exec sudo failed\n";
    }
  }
}

sub getRootSu(@){
  if(not isRoot()){
    print "## rerunning as root with su\n";

    my $user = getUsername();
    my $cmd = shellQuote($0, @_);
    $cmd = "SUDO_USER=$user $cmd";

    if(isSimulate()){
      return;
    }else{
      exec "su", "-c", $cmd or die "ERROR: exec su failed\n";
    }
  }
}

sub readConfDir($){
  my ($confDir) = @_;

  my @files = globFiles "$confDir/*";
  my %conf = map {basename($_) => [readFileChomp($_)]} @files;
  return %conf;
}

sub installFromGit($$){
  my ($gitUrl, $installActionSub) = @_;
  my $name = extractNameFromGitUrl $gitUrl;

  my $dir = getSrcCache() . "/$name";

  if(isSimulate()){
    print " install: $gitUrl\n";
    if(defined $installActionSub and $$MODULE_AVAIL{'B::Deparse'}){
      my $str = B::Deparse->new()->coderef2text($installActionSub);
      my @lines = split /[\r\n]+/, $str;
      if($lines[0] =~ /^{$/ and $lines[-1] =~ /^}$/){
        shift @lines;
        pop @lines;
      }
      @lines = grep {$_ !~ /^\s*use (warnings|strict);$/} @lines;
      @lines = map {s/^    /   # /; $_} @lines;
      $str = join '', map {"$_\n"} @lines;
      print $str;
    }
    return;
  }

  if(not -d $dir){
    runUser "mkdir", "-p", $dir;
    runUser "git", "-C", $dir, "clone", $gitUrl, ".";
  }

  if(-d "$dir/.git"){
    runUser "git", "-C", $dir, "pull";
  }else{
    die "ERROR: $dir exists but is not a git repo\n";
  }

  if(not defined $installActionSub){
    my @files = globFiles "$dir/*";
    my @cabalFiles = grep {/^$dir\/.*\.cabal$/} @files;
    my @installCmds = grep {-x $_ and $_ =~ /^$dir\/install/} @files;
    my @goModFiles = grep {/^$dir\/go\.mod$/} @files;
    if(@cabalFiles > 0){
      $installActionSub = sub{
        my ($dir) = @_;
        runUser("sh", "-c", "cd $dir && cabal install -j");
      };
    }elsif(@goModFiles > 0){
      $installActionSub = sub{
        my ($dir) = @_;
        runUser "go", "install", "-C", $dir;
      };
    }elsif(tryrunSilent("make", "-C", $dir, "-n", "all")){
      $installActionSub = sub{
        my ($dir) = @_;
        runUser("make", "-C", $dir, "-j", "all");
        run("sudo", "make", "-C", $dir, "install");
      };
    }elsif(tryrunSilent("make", "-C", $dir, "-n")){
      $installActionSub = sub{
        my ($dir) = @_;
        runUser("make", "-C", $dir, "-j");
        run("sudo", "make", "-C", $dir, "install");
      };
    }elsif(@installCmds == 1){
      $installActionSub = sub{
        my ($dir) = @_;
        runUserNoPty("sh", "-c", "cd $dir && $installCmds[0]")
      };
    }else{
      die "### no install file in $dir";
    }
  }

  &$installActionSub($dir);
}

sub removeSrcCache($){
  my ($name) = (@_);
  my $srcCache = getSrcCache();
  run "rm", "-rf", "$srcCache/$name";
}

sub removeGitSrcCache($){
  my ($gitUrl) = (@_);
  my $name = extractNameFromGitUrl $gitUrl;
  removeSrcCache $name;
}

sub extractNameFromGitUrl($){
  my ($gitUrl) = (@_);
  my $name;
  if($gitUrl =~ /(?:^|\/)   ([a-zA-Z0-9_\.\-]+)$/x){
    $name = $1;
    if($name =~ /^(.+)\.git$/){
      $name = $1;
    }
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

sub versionCmp($$){
  my ($v1, $v2) = @_;
  if(not defined $v1 and not defined $v2){
    return 0;
  }elsif(not defined $v1){
    return -1;
  }elsif(not defined $v2){
    return 1;
  }
  my @segmentsV1 = split /\./, $v1;
  my @segmentsV2 = split /\./, $v2;
  my $maxSegments = 0+@segmentsV1 > 0+@segmentsV2 ? 0+@segmentsV1 : 0+@segmentsV2;

  for(my $i=0; $i<$maxSegments; $i++){
    my $segV1 = $i <= $#segmentsV1 ? $segmentsV1[$i] : 0;
    my $segV2 = $i <= $#segmentsV2 ? $segmentsV2[$i] : 0;
    my $numV1 = $segV1 =~ /^(\d+)/ ? $1 : 0;
    my $numV2 = $segV2 =~ /^(\d+)/ ? $1 : 0;

    my $numCmp = $numV1 <=> $numV2;
    if($numCmp != 0){
      return $numCmp;
    }
    my $strCmp = $segV1 cmp $segV2;
    if($strCmp != 0){
      return $strCmp;
    }
  }

  return 0;
}

sub joinLines(@){
  return join '', map {$_ =~ s/[\r\n]+$//; "$_\n"} @_;
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
