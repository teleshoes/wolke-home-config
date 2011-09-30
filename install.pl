#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;

my @utils = (
#TOP
'header' => "",
  'xx' => 'q' => 'quit',
'header' => "",
  'xx' => 'w' => 'license',
'header' => "",
  'xx' => 'e' => 'execute_all_in_order',
#BOTTOM
'header' => "Installs:",
  '14' => '1' => 'add_ppas',
  '15' => '2' => 'sync_apt_cache',
  '16' => '3' => 'install_free_packages',
  '17' => '4' => 'install_evil_packages',
  '18' => '5' => 'remove_bad_packages',
  '19' => '6' => 'upgrade',
  '20' => '7' => 'piano',
  '21' => '8' => 'haskell_modules',
  '22' => '9' => 'perl_modules',

'header' => "System settings and hardware:",
  '06' => 'a' => 'host_name',
  '07' => 's' => 'setup_partitions',
  '08' => 'd' => 'ext_journal_commit',
  '09' => 'f' => 'bash_completion',
  '10' => 'g' => 'escribe',
  '11' => 'h' => 'disable_bluetooth',
  '12' => 'j' => 'sudoers_hacks',
  '13' => 'k' => 'setup_printers',
  '23' => 'l' => 'grub_config',

'header' => "User settings:",
  '01' => 'z' => 'wolke_home_config',
  '02' => 'x' => 'configure_keyboard',
  '03' => 'c' => 'udev_rules',
  '04' => 'v' => 'gconf_settings',
  '05' => 'b' => 'desktop_config',
  'xx' => 'n' => 'fix_compiz',
);

my $DIR = `echo -n \$HOME/utils`;
chdir $DIR;

#keyboard character -> subroutine name
my %keys;

#list of subroutines in the order described in the left column
my @order;

my @headers;
#each section contains an array of 3-element arrays
my @sections;

sub parse_utils();
sub ui_cmd_prompt();
sub handleCmd($);

sub ask($);
sub callMagicSub($);

parse_utils();

for my $arg(@ARGV){
  if(length $arg == 1){
    handleCmd $arg;
  }else{
    callMagicSub $arg;
  }
}

exit 0 if @ARGV > 0;

while(1){
  ui_cmd_prompt();
}
exit 0;


sub quit(){
  exit 0;
}
sub license(){
  print get_license_text();
}
sub execute_all_in_order(){
  my $len = length (1 + scalar @order);
  my $i=1;
  for my $sub(@order){
    print ' 'x($len-(length $i)).$i++." $sub\n";
  }

  print "\n\nExecuting the above, in the above order\n";
  print "Pressing enter without typing a response skips each step\n";
  for my $sub(@order){
    $sub =~ s/([0-9a-zA-Z_-]+).*/$1/sxi;
    callMagicSub $sub;
  }
}

###############################
###############################
###############################
#D%ui%

#LOAD SUBROUTINE BLACK MAGIC
#Takes a subrouting name and returns a
#subroutine reference to a real sub, or undef if
#no suitable sub can be found
#
#Never dies -> Lives forever
sub magicSub($){
    my $name = shift;
    my $magic;

    eval{
        $magic = \&{$name};
    };

    if($@ or not defined &{$magic}){
        return undef;
    }else{
        return $magic;
    }
};

sub callMagicSub($){
  my $sub_name = shift;
  my $sub = magicSub $sub_name;
  if($sub){
    print "running subroutine: $sub_name\n";
    &{$sub}();
  }elsif(-x "$DIR/$sub_name"){
    print "running script: $DIR/$sub_name\n";
    system "$DIR/$sub_name";
  }else{
    die "unknown subroutine: $sub_name\n";
  }
}

#parse the above into 3 different data structures:
# a hash of keys->subroutines,
# an array of all the subroutines in the order they should run,
# a 3-d array of the above split into sections delimited by 'header's:
#  there is one array for each of the 5 sections,
#  each of which contains arrays of 3 elements for each line
sub parse_utils(){
  my $sectionIndex = -1;
  for(my $i=0; $i<@utils; $i++){
    if($utils[$i] eq 'header'){
      $i++;
      $sectionIndex++;
      $headers[$sectionIndex] = $utils[$i];
      next;
    }

    my $index = $utils[$i];
    $i++;
    my $key = $utils[$i];
    $i++;
    my $sub = $utils[$i];

    if(defined $keys{$key}){
      my $old_sub = $keys{$key};
      die "Duplicate bindings for key $key: $sub and $old_sub\n";
    }
    $keys{$key} = $sub;
    if($index =~ /^\d+$/){
      $order[$index-1] = $sub;
    }
  
    my @arr = ($index, $key, $sub);
    push @{$sections[$sectionIndex]}, \@arr;
  }
}

sub key(){
  ReadMode 3;
  my $key = ReadKey 0;
  ReadMode 0;
  return $key;
}

sub ui_cmd_prompt(){
  print_utils_ui();
  
  print "Press a key (dont worry, we'll double-check again after): ";
  handleCmd key();
}

sub handleCmd($){
  my $key = shift;
  if($key eq "\n"){
    return;
  }
  print "\n\n";
  my $sub = $keys{$key};
  if(defined $sub){
    $sub =~ s/^([a-zA-Z0-9_-]+).*$/$1/sxi;
    callMagicSub $sub;
  }else{
    print "Unrecognized key: '$key'\n";
  }
  print "\n";
  print "press any key to continue";
  key();
}

sub ask($){
  my $msg = shift;
  print "\n\n$msg [y/N] ";
  my $response = <STDIN>;
  chomp $response;
  if(lc $response eq 'y'){
    return 1;
  }else{
    print "skipped\n";
    return 0;
  }
}

sub get_term_width(){
  my ($width) = Term::ReadKey::GetTerminalSize;
  return $width;
}

sub print_section($$$$\@\@\@){
  my $term_width = shift;
  my $left_header = shift;
  my $middle_header = shift;
  my $right_header = shift;
  my @left = @{shift()};
  my @middle = @{shift()};
  my @right = @{shift()};

  my $len;
  my $max_left_len = length $left_header;
  my $max_middle_len = length $middle_header;
  my $max_right_len = length $right_header;
  for my $arr(@left){
    my ($index, $key, $sub) = @{$arr};
    my $len = 3+length $sub;
    $max_left_len = $len if $len > $max_left_len;
  }
  for my $arr(@middle){
    my ($index, $key, $sub) = @{$arr};
    my $len = 3+length $sub;
    $max_middle_len = $len if $len > $max_middle_len;
  }
  for my $arr(@right){
    my ($index, $key, $sub) = @{$arr};
    my $len = 3+length $sub;
    $max_right_len = $len if $len > $max_right_len;
  }


  if($left_header or $middle_header or $right_header){
    $left_header .= ' 'x(1+$max_left_len-length $left_header);
    $middle_header .= ' 'x(1+$max_middle_len-length $middle_header);
    $right_header .= ' 'x(1+$max_right_len-length $right_header);

    $len = $term_width - length "|$left_header|$middle_header|$right_header|";
    $left_header .= ' ' x($len/3) . ($len%3>0?' ':'');
    $middle_header .= ' ' x($len/3);
    $right_header .= ' ' x($len/3) . ($len%3==2?' ':'');

    print "|$left_header|$middle_header|$right_header|\n";
  }

  my $max_size = 0;
  $max_size = @left if @left > $max_size;
  $max_size = @middle if @middle > $max_size;
  $max_size = @right if @right > $max_size;
  for(my $i=0; $i<$max_size; $i++){
    my $left_entry='';
    my $middle_entry='';
    my $right_entry='';

    if($i < @left){
      $left_entry = " " . ${$left[$i]}[1] . ": " . ${$left[$i]}[2];
    }
    if($i < @middle){
      $middle_entry = " " . ${$middle[$i]}[1] . ": " . ${$middle[$i]}[2];
    }
    if($i < @right){
      $right_entry = " " . ${$right[$i]}[1] . ": " . ${$right[$i]}[2];
    }

    my $lspc = 1+$max_left_len - length $left_entry;
    my $mspc = 1+$max_middle_len - length $middle_entry;
    my $rspc = 1+$max_right_len - length $right_entry;
    
    my $sum = $lspc + $mspc + $rspc;
    my $lim = $term_width -
      length "|$left_entry|$middle_entry|$right_entry|";
    while(($lspc > 0 or $mspc > 0 or $rspc > 0) and $sum > $lim){
      if($rspc > 0){
        $rspc--;
      }elsif($mspc > 0){
        $mspc--;
      }elsif($lspc > 0){
        $lspc--;
      }
      $sum = $lspc + $mspc + $rspc;
    }

    $left_entry .= ' 'x$lspc;
    $middle_entry .= ' 'x$mspc;
    $right_entry .= ' 'x$rspc;

    $len = length "|$left_entry|$middle_entry|$right_entry|";
    $len = $term_width - $len;
    $left_entry .= ' ' x($len/3) . ($len%3>0?' ':'');
    $middle_entry .= ' ' x($len/3);
    $right_entry .= ' ' x($len/3) . ($len%3==2?' ':'');
    
    print "|$left_entry|$middle_entry|$right_entry|\n";
  }


}
sub print_utils_ui(){
  system "clear";
  my $term_width = get_term_width();

  print '-'x$term_width . "\n";
  print_section($term_width,
    $headers[0], $headers[1], $headers[2],
    @{$sections[0]}, @{$sections[1]}, @{$sections[2]});
  print '-'x$term_width . "\n";
  print_section($term_width,
    $headers[3], $headers[4], $headers[5],
    @{$sections[3]}, @{$sections[4]}, @{$sections[5]});
  print '-'x$term_width . "\n";
}


sub get_license_text(){
"   Installation and config script, Copyright (c) 2011 Elliot Wolk

    This script is licensed under GNU GPL version 3.0 or above
    #####################################################################
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    #####################################################################
";
}

