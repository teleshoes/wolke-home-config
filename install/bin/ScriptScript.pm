package ScriptScript;
use warnings;
use strict;
use String::ShellQuote;
use File::Temp 'tempfile';
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(setOpts);
our @EXPORT = qw( run tryrun
                  shell tryshell
                  runSys runUser
                  proc procLines
                  getInstallPath
                  cd chownUser
                  which
                  writeFile tryWriteFile
                  readFile tryReadFile
                  replaceLine replaceOrAddLine
                  editFile editFileConf
                  getRoot getRootSu
                  getUsername
                  guessBackupDir
                  relToScript
                  readConf readConfDir
                  installFromDir installFromGit aptSrcInstall
                );

sub setOpts($);
sub deathWithDishonor();
sub runProto($$);
sub run(@);
sub tryrun(@);
sub shell(@);
sub tryshell(@);
sub runUser(@);
sub runSys(@);
sub proc(@);
sub procLines(@);
sub getUsername();
sub getInstallPath($);
sub which($);
sub cd($);
sub chownUser($);
sub writeFileProto($);
sub writeFile($$);
sub tryWriteFile($$);
sub readFileProto($);
sub readFile($);
sub tryReadFile($);
sub replaceLine($$$);
sub replaceOrAddLine($$$);
sub editFile($$;$);
sub editFileConf($$$);
sub getRoot(@);
sub getRootSu(@);
sub guessBackupDir();
sub relToScript($);
sub readConf($);
sub readConfDir($);
sub installFromDir($;$$);
sub installFromGit($;$);
sub aptSrcInstall($$);


my $opts = {
  putCommand => 1,
  runCommand => 1,
  verbose    => 1,
  };

sub setOpts($) {
    my %new = (%$opts, %{$_[0]});
    $opts = \%new;
}

sub deathWithDishonor() {
    print STDERR "## command failed, exiting\n";
    exit 1;
}

sub runProto($$) {
    my ($esc, $dieOnError) = @_;
    sub {
        my $cmd = join ' ', &$esc(@_);

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        my $pid = open my $fh, "-|";
        if(not $pid) {
            open(STDERR, ">&STDOUT");
            exec $cmd or exit 1;
        } else {
            if($opts->{verbose}) {
                while(my $line = <$fh>) {
                    print "# " if $opts->{putCommand};
                    chomp $line;
                    print "$line\n";
                }
            }
            close $fh;
            deathWithDishonor if $? != 0 and $dieOnError;
        }
    }
}
sub id(@){@_}

sub run     (@) { &{runProto \&shell_quote, 1}(@_) }
sub tryrun  (@) { &{runProto \&shell_quote, 0}(@_) }
sub shell   (@) { &{runProto \&id         , 1}(@_) }
sub tryshell(@) { &{runProto \&id         , 0}(@_) }
sub runUser (@) { run("su", getUsername(), "-c", "@_") }

sub runSys  (@) {
    print "@_\n";
    system @_;
    deathWithDishonor if $? != 0;
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

sub getUsername() {
    my $user = $ENV{SUDO_USER} || $ENV{USER};
    if(not $user or $user eq "root") {
        print STDERR "ERROR: USER or SUDO_USER must be set and not root";
        exit 1;
    }
    $user
}

sub getInstallPath($) {
    return "$ENV{HOME}/install/$_[0]";
}

sub which($) {
    return proc "which", @_;
}

sub cd($) {
    my $path = shift;
    my $cmd = "cd $path";

    print "$cmd\n" if $opts->{putCommand};
    return     unless $opts->{runCommand};

    chdir $path or deathWithDishonor;
}

sub chownUser($) {
    my $path = shift;
    my $user = getUsername;
    my $cmd = "chown -R $user. $path";

    print "$cmd\n" if $opts->{putCommand};
    return     unless $opts->{runCommand};

    run "sudo", "chown", "-R", "$user.", $path;
}

sub writeFileProto($) {
    my ($dieOnError) = @_;
    sub {
        my ($name, $cnts) = @_;

        my $escname = shell_quote $name;

        my $delim = "EOF";
        while($cnts =~ /^$delim$/m) { $delim .= "F" }

        chomp $cnts;

        my $cmd = join "\n"
          , "( cat << \"$delim\""
          , $cnts
          , $delim
          , ") > $escname";

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        my $opened = open my $fh, ">", $name;
        if($opened) {
            print $fh "$cnts\n";
            close $fh;
        } elsif($dieOnError) {
            deathWithDishonor
        }
    }
}
sub writeFile    ($$) { &{writeFileProto 1}(@_) }
sub tryWriteFile ($$) { &{writeFileProto 0}(@_) }

sub readFileProto($) {
    my ($dieOnError) = @_;
    sub {
        my ($name) = @_;

        my $escname = shell_quote $name;

        my $opened = open my $fh, "<", $name;
        if($opened) {
            if(wantarray) {
                my @cnts = <$fh>;
                close $fh;
                return @cnts;
            } else {
                local $/;
                my $cnts = <$fh>;
                close $fh;
                return $cnts;
            }
        } elsif($dieOnError) {
            print STDERR "## failed to read file $escname , exiting\n";
            exit 1;
        }
    }
}
sub readFile    ($) { &{readFileProto 1}(@_) }
sub tryReadFile ($) { &{readFileProto 0}(@_) }

sub replaceLine($$$) {
    my (undef, $old, $new) = @_;
    if($_[0] =~ /^#? ?$old/m) {
        $_[0] =~ s/^#? ?$old.*/$new/m;
    }
    $&
}

sub replaceOrAddLine($$$) {
    my (undef, $old, $new) = @_;
    if($_[0] =~ /^#? ?$old/m) {
        $_[0] =~ s/^#? ?$old.*/$new/m;
    } else  {
        chomp $_[0];
        $_[0] .= "\n";
        $_[0] =~ s/\n+$/$&$new\n/;
    }
    $&
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
        my $escname = shell_quote $name;
        my $escpatch = defined $patchname ? " " .shell_quote $patchname : "";
        print STDERR "## editFile $escname$escpatch: "; 
        print STDERR "edit function failed, exiting\n";
        exit 1;
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
            my $delim = "EOF";
            while($newpatch =~ /^$delim$/m) { $delim .= "F" }

            chomp $newpatch;
            my $cmd = join "\n"
              , "$escpatchcmd - << \"$delim\""
              , $newpatch
              , $delim;

            shell $cmd;
        }
    }
}

sub editFileConf($$$) {
    my ($name, $patchname, $config) = @_;
    editFile $name, $patchname, sub {
        my $cnts = shift;
        for my $key(keys %$config){
          replaceOrAddLine $cnts, $key, "$key=$$config{$key}";
        }
        $cnts
    };
}

sub getRoot(@) {
    if(`whoami` ne "root\n") {
        print "## rerunning as root\n";

        my $cmd = "if [ `whoami` != \"root\" ]; then exec sudo $0 @_; fi";

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        exec "sudo", $0, @_ or print "## failed to sudo, exiting";
        exit 1;
    }
}

sub getRootSu(@) {
    if(`whoami` ne "root\n") {
        print "## rerunning as root\n";

        my $user = getUsername();
        my $cmd = ""
	  . "if [ `whoami` != \"root\" ]; "
	  .   "then exec su -c \"SUDO_USER=$user $0 @_\" ; "
	  . "fi"
	  ;

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        exec "su", "-c", "SUDO_USER=$user $0 @_"
	  or print "## failed to su, exiting";
        exit 1;
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

sub relToScript($) {
    my ($path) = @_;

    $0 =~ /\/[^\/]+$/;
    "$`/$path"
}

sub readConf($) {
    my ($file) = @_;

    my @lines = readFile(relToScript $file);
    chomp @lines;
    @lines
}

sub readConfDir($) {
    my ($reldir) = @_;

    my $dir = relToScript $reldir;
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
        run "mkdir", "-p", $dir;
        cd $dir;
        run "git", "clone", $gitUrl, ".";
        chownUser $dir;
    }
    cd $dir;
    tryrun qw(git pull) if -d ".git";

    if(defined $cmd){
      shell $cmd;
    }else{
      my @ls = split "\n", `ls -1`;
      if(grep {/\.cabal$/} @ls) {
          shell "cabal install";
      } elsif(system("make -n all >/dev/null 2>&1") == 0) {
          shell "make -j all";
          shell "sudo make install";
      } elsif(system("make -n >/dev/null 2>&1") == 0) {
          shell "make -j";
          shell "sudo make install";
      } elsif(grep {/^install/} @ls) {
          shell "./install*";
      } else {
          print STDERR "### no install file in $dir , exiting\n";
          exit 1;
      }
    }
}

sub installFromGit($;$) {
  my ($gitUrl, $cmd) = (@_, undef);
  my $repo = $1 if $gitUrl =~ /\/([^\/]*?)(\.git)?$/;
  my $srcCacheDir = "$ENV{HOME}/.src-cache";
  if(not -d $srcCacheDir){
    run "mkdir", "-p", $srcCacheDir;
    chownUser $srcCacheDir;
  }
  installFromDir "$ENV{HOME}/.src-cache/$repo", $gitUrl, $cmd;
}

sub aptSrcInstall($$) {
    my ($package, $whichdeb) = @_;
    shell "sudo apt-get -y build-dep $package";
    my $srcdir = "$ENV{HOME}/.src-cache/$package";
    shell "mkdir $srcdir" unless -d $srcdir;
    cd $srcdir;
    shell "apt-get -b source $package";
    for my $file (split "\n", `ls -1`) {
        if($file =~ /\.deb$/ && $file =~ /$whichdeb/) {
            shell "sudo dpkg -i $file";
        }
    }
}

1;
