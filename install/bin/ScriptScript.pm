package ScriptScript;
use warnings;
use strict;
use String::ShellQuote;
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(setOpts);
our @EXPORT = qw( run tryrun
                  shell tryshell
                  cd
                  writeFile tryWriteFile
                  readFile tryReadFile
                  editFile replaceLine replaceOrAddLine
                  getRoot
                  getUsername
                  guessBackupDir
                  relToScript
                  readConf readConfDir
                  installFromDir
                );

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

        my $pid = open FH, "-|";
        if($pid) {
            if($opts->{verbose}) {
                while(my $line = <FH>) {
                    print "# " if $opts->{putCommand};
                    chomp $line;
                    print "$line\n";
                }
            }
            close FH;
            deathWithDishonor if $? != 0 and $dieOnError;
        } else {
            open(STDERR, ">&STDOUT");
            exec $cmd or exit 1;
        }
    }
}
sub run     (@) { &{runProto \&shell_quote, 1}(@_) }
sub tryrun  (@) { &{runProto \&shell_quote, 0}(@_) }
sub shell   (@) { &{runProto sub{@_}      , 1}(@_) }
sub tryshell(@) { &{runProto sub{@_}      , 0}(@_) }

sub cd($) {
    my $path = join ' ', shell_quote @_;
    my $cmd = "cd $path";

    print "$cmd\n" if $opts->{putCommand};
    return     unless $opts->{runCommand};

    chdir $path or deathWithDishonor;
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
          , "( cat << $delim"
          , $cnts
          , $delim
          , ") > $escname";

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        my $opened = open FH, ">", $name;
        if($opened) {
            print FH "$cnts\n";
            close FH;
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

        my $opened = open FH, "<", $name;
        if($opened) {
            if(wantarray) {
                my @cnts = <FH>;
                close FH;
                return @cnts;
            } else {
                local $/;
                my $cnts = <FH>;
                close FH;
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

sub editFile($$;$) {
    my ($name, $patchname, $edit);
    ($name, $edit) = @_             if @_ == 2;
    ($name, $patchname, $edit) = @_ if @_ == 3;

    my $escname = shell_quote $name;
    my $patchcmd = "patch -fr - $escname";
    # TODO be quieter if patches match, surreptitious revert to tmpfile first?

    # revert previous patch if one exists
    my $patchfile = shell_quote "$name.$patchname.patch" if defined $patchname;
    if (defined $patchfile and -f $patchfile) {
        my $revcmd = "$patchcmd $patchfile -R";
        shell "$revcmd --dry-run --quiet";
        shell $revcmd;
    }

    # create and apply new patch
    my $read  = readFile $name;
    my $tmp   = $read;
    my $write = &$edit($tmp);
    deathWithDishonor unless defined $write;

    if($write eq $read) {
        shell "rm $patchfile" if defined $patchfile and -e $patchfile;
    } else {
        my $diff;
        my $pid = open FHIN, "-|";
        if($pid) {
            local $/;
            $diff = <FHIN>;
            close FHIN;
        } else {
            open(STDERR, ">&STDOUT");
            open FHOUT, "|-", "diff", $name, "-";
            print FHOUT $write;
            close FHOUT;
            exit;
        }

        if(defined $patchfile) {
            writeFile $patchfile, $diff;
            shell "$patchcmd $patchfile";
        } else {
            my $delim = "EOF";
            while($diff =~ /^$delim$/m) { $delim .= "F" }

            chomp $diff;

            my $cmd = join "\n"
              , "$patchcmd << $delim"
              , $diff
              , $delim;

            shell $cmd;
        }
    }
}

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


sub getRoot(@) {
    if(`whoami` ne "root\n") {
        print "## rerunning as root\n";

        my $cmd = 'if [ `whoami` != "root" ]; then exec sudo $0 ; fi';

        print "$cmd\n" if $opts->{putCommand};
        return     unless $opts->{runCommand};

        exec "sudo", $0, @_ or print "## failed to sudo, exiting";
        exit 1;
    }
}

sub getUsername() {
    my $user = $ENV{SUDO_USER} || $ENV{USER};
    if(not $user or $user eq "root") {
        print STDERR "ERROR: USER or SUDO_USER must be set and not root";
        exit 1;
    }
    $user
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

    my $scriptdir = `dirname $0`;
    chomp $scriptdir;

    "$scriptdir/$path"
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

sub installFromDir($) {
    my ($dir) = @_;
    cd $dir;
    run qw(git pull) if -d ".git";

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
        shell "install*";
    } else {
        print STDERR "### no install file in $dir , exiting\n";
        exit 1;
    }
}

1;
