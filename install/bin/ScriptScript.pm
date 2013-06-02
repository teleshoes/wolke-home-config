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
                  editFile
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
                close FH;
            } else {
                waitpid $pid, 0;
            }
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

sub editFile($$) {
    my ($name, $edit) = @_;

    my $read  = readFile $name;
    my $write = &$edit($read);

    my $diff = "";
    if($write ne $read) {
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

        my $escname = shell_quote $name;

        my $delim = "EOF";
        while($write =~ /^$delim$/m) { $delim .= "F" }

        chomp $diff;

        my $cmd = join "\n"
          , "patch $escname << $delim"
          , $diff
          , $delim;

        shell $cmd;
    }
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
    if(`ls` =~ /\.cabal$/) {
        shell "cabal install";
    } elsif(`ls` =~ /^install/) {
        shell "install*";
    } elsif(system "make -qn all") {
        shell "make -j all";
        shell "sudo make install";
    } elsif(system "make -qn") {
        shell "make -j";
        shell "sudo make install";
    } else {
        print STDERR "### no install file in $dir , exiting";
        exit 1;
    }
}

1;
